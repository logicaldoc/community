package com.logicaldoc.core.automation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
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

	protected static Logger log = LoggerFactory.getLogger(FolderTool.class);

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

		try {
			TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			Tenant tenant = tenantDao.findById(tenantId);
			url += "display?tenant=" + tenant.getName() + "&folderId=" + folderId;
			return url;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
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
	 * @throws PersistenceException error at database level 
	 */
	public String getPath(Long folderId) throws PersistenceException {
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

	/**
	 * Saves / updates a folder into the database
	 * 
	 * @param folder the folder to save
	 */
	public void store(Folder folder) {
		store(folder, null);
	}

	/**
	 * Saves / updates a folder into the database
	 * 
	 * @param folder the folder to save
	 * @param username the user in whose name the method is run
	 */
	public void store(Folder folder, String username) {
		User user = new SecurityTool().getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolder(folder);
		transaction.setUser(user);

		try {
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			folderDao.store(folder, transaction);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}
	}

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param folder the folder to initialize
	 */
	public void initialize(Folder folder) {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		folderDao.initialize(folder);
	}

	/**
	 * Delete a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void delete(long folderId, String username) throws Exception {
		User user = new SecurityTool().getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolderId(folderId);
		transaction.setUser(user);

		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		folderDao.delete(folderId, transaction);
	}

	/**
	 * Moves a folder into a target folder
	 * 
	 * @param folder the folder to move
	 * @param targetPath the full path of the target folder(if it does not exist
	 *        it will be created)
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void move(Folder folder, String targetPath, String username) throws Exception {
		User user = new SecurityTool().getUser(username);

		Folder target = createPath(null, targetPath, username);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolder(folder);
		transaction.setUser(user);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		fdao.move(folder, target, transaction);
	}

	/**
	 * Copies a folder into another folder
	 * 
	 * @param source the folder to copy
	 * @param targetPath the full path of the target folder(if it does not exist
	 *        it will be created)
	 * @param foldersOnly true if only the folders tree has to be copied; if
	 *        false, the documents will also be copied
	 * @param securityOption How to assign the security policies to the newly
	 *        created folders:
	 *        <ul>
	 *        <li><b>null</b> or <b>none</b>: empty security policies</li>
	 *        <li><b>inherit</b>: the new folder will point to the parent for
	 *        the security policies</li>
	 *        <li><b>replicate</b>: the new folder will have a copy of the
	 *        security policies of the source folder</li>
	 *        </ul>
	 * @param username the user in whose name the method is run
	 * @return The new folder created
	 * 
	 * @throws Exception a generic error happened
	 */
	public Folder copy(Folder source, String targetPath, boolean foldersOnly, String securityOption, String username)
			throws Exception {
		User user = new SecurityTool().getUser(username);

		Folder target = createPath(null, targetPath, username);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolder(source);
		transaction.setUser(user);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

		return fdao.copy(source, target, null, foldersOnly, securityOption, transaction);
	}

	/**
	 * Merges a folder into another folder
	 * 
	 * @param source the folder to merge into the target
	 * @param target the final container
	 * @param username the user in whose name the method is run
	 * 
	 * @throws Exception a generic error happened
	 */
	public void merge(Folder source, Folder target, String username) throws Exception {
		User user = new SecurityTool().getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolder(source);
		transaction.setUser(user);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		fdao.merge(source, target, transaction);
	}

	/**
	 * Creates a path, all existing nodes in the specified path will be reused
	 * 
	 * @param folder folder used to take the root folder in case the
	 *        <code>targetPath</code> represents a relative path
	 * @param targetPath absolute or relative(in relation to the document's
	 *        parent folder) path to be created
	 * @param username the user in whose name the method is run
	 * 
	 * @return the folder leaf of the path
	 */
	public Folder createPath(Folder folder, String targetPath, String username) {
		SecurityTool secTool = new SecurityTool();
		User user = secTool.getUser(username);

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);

		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder parent = folder;
		Folder newFolder = null;
		try {
			if (targetPath.startsWith("/")) {
				targetPath = targetPath.substring(1);
				/*
				 * Cannot write in the root so if the parent is the root, we
				 * have to guarantee that the first element in the path is a
				 * workspace. If not the Default one will be used.
				 */
				parent = fdao.findRoot(folder.getTenantId());
				Folder workspace = null;

				/*
				 * Check if the path contains the workspace specification
				 */
				for (Folder w : fdao.findWorkspaces(folder.getTenantId())) {
					if (targetPath.startsWith(w.getName())) {
						workspace = fdao.findById(w.getId());
						break;
					}
				}

				if (workspace == null) {
					parent = fdao.findDefaultWorkspace(folder.getTenantId());
				}
			}

			newFolder = fdao.createPath(parent, targetPath, true, transaction);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return newFolder;
	}
}