package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * The client side stub for the Document Service. This service allows r/w
 * operations on documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
@RemoteServiceRelativePath("update")
public interface UpdateService extends RemoteService {

	/**
	 * Check if the current installation has an update package available
	 * 
	 * @param userNo The current UserNo
	 * @param currentRelease The actual release
	 * @return List of informations about the available update package or null
	 */
	public GUIParameter[] checkUpdate(String userNo, String currentRelease);

	/**
	 * Downloads an update package
	 * 
	 * @param userNo The current UserNo
	 * @param id ID of the package to download
	 * @param fileName Name of the package file
	 * @param fileName Size of the package file expressed in MB
	 * @return 0 if the download was successful
	 */
	public void download(String userNo, String id, String fileName, int size);

	/**
	 * Confirms an update package
	 * 
	 * @param userNo The update file to confirm
	 * @return The path of the update folder
	 */
	public String confirm(String updateFileName) throws ServerException;

	/**
	 * Retrieves more informations from
	 * 
	 * @param updateFileName File name of the downloaded update package
	 * @return ChangeLog and Install file contents
	 */
	public String[] getNotes(String updateFileName) throws ServerException;

	/**
	 * Checks the status of the current download process
	 * 
	 * @return download status code and download progress
	 */
	public int[] checkDownloadStatus();
	
	public static class Instance {
		private static UpdateServiceAsync instance;

		public static UpdateServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(UpdateService.class);
			}
			return instance;
		}
	}
}