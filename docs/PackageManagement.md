# Enabling Package Management

This feature is necessary to add and, therefore, use additional packages to the SQL Server Compute Context. Essentially, remote package management has to be enabled for the SQL Server Instance the scripts are meant to be run. To do this, open an elevated command prompt on the SQL Server and navigate to ```RegisterRExt.exe```; whose default path is ```<SQLInstancePath>\R_SERVICES\library\RevoScaleR\rxLibs\x64\RegisterRExe.exe```. Then enable package management by executing the following command is relevant:

```
RegisterRExt.exe /installpkgmgmt /database:databasename [/instance:name] [/user:username] [/password:*|password]
```

To enable package management on a specific database, the following seems to worked for me:

```
RegisterRExt.exe /installpkgmgmt /instance:name
RegisterRExt.exe /installpkgmgmt /database:name /instance:name
```
This command create some database artifacts, including the following database roles that are used for controlling user permissions: ```rpkgs-users```, ```rpkgs-private```, and ```rpkgs-shared```. To verify that the new roles have been successfully created, execute the following command on the target database:

```{SQL}
SELECT pr.principal_id, pr.name, pr.type_desc,   
    pr.authentication_type_desc, pe.state_desc,   
    pe.permission_name, s.name + '.' + o.name AS ObjectName  
FROM sys.database_principals AS pr  
JOIN sys.database_permissions AS pe  
    ON pe.grantee_principal_id = pr.principal_id  
JOIN sys.objects AS o  
    ON pe.major_id = o.object_id  
JOIN sys.schemas AS s  
    ON o.schema_id = s.schema_id;
```

Microsoft's description did not work completely with my version of SQL Server and might be slightly deprecated. However, for future use; see [Microsoft](https://docs.microsoft.com/en-us/sql/advanced-analytics/r/r-package-how-to-enable-or-disable?view=sql-server-2017#bkmk_enable).

# Installing R Packages on SQL Server

After enabling package management, a package on the SQL Server can then be done directly in SQL Server during development

```{R}
# define packages
pkgs <- c("path/to/packages")
# install packages on sql server
rxInstallPackages(pkgs = pkgs, repos=NULL, verbose = TRUE, scope = "private", computeContext = sqlServerCC)
# list all packages currently installed on sql server
```

All packages that were installed in this manner are listed in the ```rpackages``` table.