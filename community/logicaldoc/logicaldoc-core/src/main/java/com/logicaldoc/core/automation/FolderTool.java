package com.logicaldoc.core.automation;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Utility methods to handle folders from within Velocity
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FolderTool {

	public String displayUrl(long tenantId, long folderId) {
		ContextProperties config = Context.get().getProperties();
		String url = config.getProperty("server.url");
		if (!url.endsWith("/"))
			url += "/";
		

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = tenantDao.findById(tenantId);
		
		url += "display?tenant="+tenant.getName()+"&folderId=" + folderId;
		return url;
	}
	
	public String displayUrl(Folder folder) {
		return displayUrl(folder.getTenantId(), folder.getId());
	}
	
	public String displayUrl(FolderHistory history) {
		return displayUrl(history.getTenantId(), history.getFolderId());
	}
}
