package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIKeystore;

/**
 * The client side stub for the Sign Service. This service gives all needed
 * methods to handle documents signature.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@RemoteServiceRelativePath("sign")
public interface SignService extends RemoteService {

	/**
	 * Gets the keystore's metadata of the given tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the key store
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIKeystore loadKeystore(long tenantId) throws ServerException;

	/**
	 * Discards the actual key store and generates a new one
	 * 
	 * @param keystore the key store details
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void generateNewKeystore(GUIKeystore keystore) throws ServerException;

	/**
	 * Generates a self-signes certificate for the current user.
	 * 
	 * @throws ServerException an error happened manipulating the keystore
	 */
	public void generateNewCertificate() throws ServerException;

	/**
	 * Imports an uploaded certificate
	 * 
	 * @throws ServerException an error happened manipulating the keystore
	 */
	public void importCertificate(String privateKey) throws ServerException;

	/**
	 * Delete the certificate of the current user
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteCertificate() throws ServerException;

	/**
	 * Deletes the actual key store
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteKeystore(long tenantId) throws ServerException;

	public void saveKeystore(GUIKeystore keystore) throws ServerException;

	public void imporKeystore(GUIKeystore keystore) throws ServerException;

	public boolean isVisualSignatureEnabled() throws ServerException;

	public void signDocuments(Long[] docIds, String reason, int page, String signX, String signY, String signWidth)
			throws ServerException;

	public static class Instance {

		private static SignServiceAsync inst;

		private Instance() {
		}

		public static SignServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SignService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}