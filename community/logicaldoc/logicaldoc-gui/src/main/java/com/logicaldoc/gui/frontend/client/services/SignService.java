package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 */
	public GUIKeystore loadKeystore(long tenantId) throws ServerException;

	/**
	 * Discards the actual key store and generates a new one
	 */
	public void generateNewKeystore(GUIKeystore keystore) throws ServerException;

	/**
	 * Creates a new certificate for the current user
	 */
	public void generateNewCertificate(String password) throws ServerException;

	/**
	 * Delete the certificate of the current user
	 */
	public void deleteCertificate() throws ServerException;

	/**
	 * Deletes the actual keystore
	 */
	public void deleteKeystore(long tenantId) throws ServerException;

	public void saveKeystore(GUIKeystore keystore) throws ServerException;

	public void imporKeystore(GUIKeystore keystore) throws ServerException;

	/**
	 * Signs the given documents
	 * 
	 * @param docIds The documents to be signed
	 * @param reason The reason for the signature
	 * @param password The password to use with the user's certificate
	 * @throws ServerException
	 */
	public void signDocuments(long[] docIds, String reason, String password) throws ServerException;

	public static class Instance {
		private static SignServiceAsync instance;

		public static SignServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SignService.class);
			}
			return instance;
		}
	}
}