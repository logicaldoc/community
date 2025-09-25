package com.logicaldoc.gui.frontend.client.services;

import java.util.Collection;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocuSignSettings;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * The client side stub for the DocuSign Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
@RemoteServiceRelativePath("docusign")
public interface DocuSignService extends RemoteService {

	/**
	 * Retrieves the DocuSign settings from the server.
	 * 
	 * @return the DocuSign settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocuSignSettings loadSettings() throws ServerException;

	/**
	 * Saves the settings into the database and returns the authorization URL
	 * the user must be redirected to.
	 * 
	 * @param settings The settings to save in the user's profile
	 * 
	 * @return the authorization URL
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String authorize(GUIDocuSignSettings settings) throws ServerException;

	/**
	 * Checks if the current user is authorized to interact with DocuSign
	 * 
	 * @return if you are authorized
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean isAuthorized() throws ServerException;

	/**
	 * Retrieves the list of signers of a given envelope
	 *
	 * @param envelopeId identifier of the envelope
	 * 
	 * @return the list of signer e-mails
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public Collection<String> getSigners(String envelopeId) throws ServerException;

	/**
	 * Checks if a collection of document can be put into an envelope, that is
	 * if all of them define the docusign tabs annotations.
	 * 
	 * @param docIds identifiers of the documents contained in the envelope
	 * 
	 * @return the list of documents that are not admitted
	 *
	 * @throws ServerException an error happened in the server application
	 */
	public Collection<GUIDocument> validateEnvelope(Collection<Long> docIds) throws ServerException;

	/**
	 * Creates the envelope and sends it to DocuSign
	 * 
	 * @param envelope the envelope's details
	 * 
	 * @return the identifier of the new envelope
	 *
	 * @throws ServerException an error happened in the server application
	 */
	public String sendEnvelope(GUIDocuSignSettings envelope) throws ServerException;

	public static class Instance {
		private static DocuSignServiceAsync inst;

		private Instance() {
		}
		
		public static DocuSignServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(DocuSignService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}