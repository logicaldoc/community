package com.logicaldoc.core.automation;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Utility methods to handle folders from within the Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 7.3
 */
@AutomationDictionary
public class FolderTool {

	/**
	 * Prints the URL to display the folder inside the User Interface
	 * 
	 * @param tenantId identifier of the tenant
	 * @param folderId identifier of the folder
	 * 
	 * @return the display URL
	 */
	public String displayUrl(long tenantId, long folderId) {
		ContextProperties config = Context.get().getProperties();
		String url = config.getProperty("server.url");
		if (!url.endsWith("/"))
			url += "/";

		TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = tenantDao.findById(tenantId);

		url += "display?tenant=" + tenant.getName() + "&folderId=" + folderId;
		return url;
	}

	/**
	 * Prints the URL to display the folder inside the User Interface
	 * 
	 * @param folder the folder to display
	 * 
	 * @return the display URL
	 */
	public String displayUrl(Folder folder) {
		return displayUrl(folder.getTenantId(), folder.getId());
	}

	/**
	 * Prints the URL to display the folder inside the User Interface
	 * 
	 * @param history event from which to get the folder's to display
	 * 
	 * @return the display URL
	 */
	public String displayUrl(FolderHistory history) {
		return displayUrl(history.getTenantId(), history.getFolderId());
	}

	/**
	 * Calculates the full path of a folder
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return the path
	 */
	public String getPath(Long folderId) {
		if (folderId == null)
			return "";
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		return folderDao.computePathExtended(folderId);
	}

	/**
	 * Finds the folder by it's path
	 * 
	 * @param path the path to process
	 * 
	 * @return the found folder
	 */
	public Folder findByPath(String path) {
		return findByPath(path, null);
	}

	/**
	 * Finds the folder by it's path
	 * 
	 * @param path the path to process
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the found folder
	 */
	public Folder findByPath(String path, Long tenantId) {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		return folderDao.findByPathExtended(path, tenantId != null ? tenantId : Tenant.DEFAULT_ID);
	}
}