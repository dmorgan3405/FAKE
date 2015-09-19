namespace Fake.IIS

module IISConfiguration = 

    open Microsoft.Web.Administration
    open Fake.PermissionsHelper
    open Fake.ProcessHelper

    type ApplicationPoolIdentity = 
        |LocalSystem
        |LocalService
        |ApplicationPoolIdentity
        |NetworkService

    let private toProcessModelIdentityType = function
                |LocalSystem -> ProcessModelIdentityType.LocalSystem 
                |LocalService -> ProcessModelIdentityType.LocalService
                |ApplicationPoolIdentity -> ProcessModelIdentityType.ApplicationPoolIdentity
                |NetworkService -> ProcessModelIdentityType.NetworkService
    
    type AppPoolConfig =  {
        AppPoolName : string
        Runtime     : string
        Allow32on64 : bool
        Identity    : ApplicationPoolIdentity
    }

    type SiteConfig = {
        SiteName        : string
        Binding         : string
        PhysicalPath    : string
        AppPoolName     : string
        Id              : int64 option
        Protocol        : string
    }

    /// Default AppPoolOptions
    let ApplicationPoolOptions = {
        AppPoolName = ""; 
        Runtime = "v4.0"; 
        Allow32on64 = false; 
        Identity = ApplicationPoolIdentity.ApplicationPoolIdentity 
    }

    //Default SiteOptions
    let SiteOptions = {
        SiteName = "";
        Binding  ="";
        PhysicalPath ="";
        AppPoolName  ="";
        Id = None;
        Protocol = "http";
    }

    let private existingAppPool (appPoolName:string) (mgr : ServerManager) = 
        let appPool = mgr.ApplicationPools.[appPoolName]
        match (appPool) with
        | null ->None
        | _ -> Some(appPool)

    let private FindOrCreateAppPool (appPoolName:string)  (mgr : ServerManager) = 
        let appPool = existingAppPool appPoolName mgr
        match (appPool) with
        | None -> mgr.ApplicationPools.Add(appPoolName)
        | Some(pool) -> pool

    let private existingSite (siteName:string) (mgr : ServerManager) =
        let site = mgr.Sites.[siteName]
        match (site) with
        | null ->None
        | _ -> Some(site)
    
    let private FindOrCreateSite (siteOptions:SiteConfig)  (mgr : ServerManager) = 
        let site = existingSite siteOptions.SiteName mgr
        match (site) with
        | None -> mgr.Sites.Add(siteOptions.SiteName, siteOptions.Protocol, siteOptions.Binding, siteOptions.PhysicalPath)
        | Some(pool) -> pool
   
    let private doWithManager (f : ServerManager->unit) (mgr : ServerManager option) =
        match mgr with
        | Some m -> f m
        | None ->
            let m = new ServerManager()
            f m
            m.CommitChanges()

    let ApplicationPool (setParams:AppPoolConfig -> AppPoolConfig) (mgr : ServerManager) = 
        let appPoolOptions = setParams(ApplicationPoolOptions)
        let appPool = FindOrCreateAppPool appPoolOptions.AppPoolName mgr
        appPool.ProcessModel.IdentityType <- toProcessModelIdentityType <| appPoolOptions.Identity

    let SetPhysicalPath (virtualPath : string) physicalPath (siteName : string) (manager : ServerManager option) =
        doWithManager (fun m ->
            let site = m.Sites.[siteName]
            let app = site.Applications.[virtualPath]
            let virtDir = app.VirtualDirectories.[virtualPath]
            virtDir.PhysicalPath <- physicalPath
        ) manager

    let AddBindingToSite (bindingInformation : string) (bindingProtocol : string) (siteName : string) (manager : ServerManager option) =
        doWithManager (fun m ->
            let site = m.Sites.[siteName]
            match site.Bindings |> Seq.exists( fun b -> b.Protocol = bindingProtocol && b.BindingInformation = bindingInformation) with
            | false -> site.Bindings.Add(bindingInformation, bindingProtocol) |> ignore
            | true -> ()
        ) manager

    let Site setParams (mgr : ServerManager) =
        let siteOptions = setParams(SiteOptions)
        let mutable site = FindOrCreateSite siteOptions mgr
        
        SetPhysicalPath "/" siteOptions.PhysicalPath siteOptions.SiteName (Some mgr)
        AddBindingToSite siteOptions.Binding siteOptions.Protocol siteOptions.SiteName (Some mgr)
        site.ApplicationDefaults.ApplicationPoolName <- siteOptions.AppPoolName
        match (siteOptions.Id) with
        | Some id -> site.Id <- id
        | None -> ()
        site