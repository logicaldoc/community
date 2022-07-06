package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIPatch;

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
	 * 
	 * @return List of informations about the available update package or null
	 */
	public GUIParameter[] checkUpdate(String userNo, String currentRelease);

	/**
	 * Check if the current installation has patches available
	 * 
	 * @param userNo The current UserNo
	 * @param currentRelease The actual release
	 * 
	 * @return List of available patches
	 */
	public GUIPatch[] checkPatch(String userNo, String currentRelease);

	void downloadUpdate(String userNo, String id, String fileName, long fileSize);

	void downloadPatch(String userNo, String id, String fileName, long fileSize);
	
	/**
	 * Confirms an update package
	 * 
	 * @param updateFileName The update file to confirm
	 * 
	 * @return The path of the update folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String confirmUpdate(String updateFileName) throws ServerException;

	/**
	 * Confirms a patch
	 * 
	 * @param patchFileName The patch file to confirm
	 * 
	 * @return The path of the patch folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String confirmPatch(String patchFileName) throws ServerException;

	/**
	 * Retrieves more informations from the update
	 * 
	 * @param updateFileName File name of the downloaded update package
	 * 
	 * @return ChangeLog and Install file contents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] getUpdateNotes(String updateFileName) throws ServerException;

	/**
	 * Retrieves more informations from the patch
	 * 
	 * @param patchFileName File name of the downloaded patch package
	 * 
	 * @return ChangeLog and Install file contents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] getPatchNotes(String patchFileName) throws ServerException;
	
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
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}