package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.beans.GUITenant;

/**
 * The client side stub for the Tenant Service. This service allows r/w
 * operations on tenants.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
@RemoteServiceRelativePath("tenant")
public interface TenantService extends RemoteService {
	/**
	 * Deletes a specific tenant by its ID
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long tenantId) throws ServerException;

	/**
	 * Saves/Updates a given tenant
	 * 
	 * @param tenant the tenant to save
	 * 
	 * @return the saved tenant
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITenant save(GUITenant tenant) throws ServerException;

	/**
	 * Loads a given tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the tenant retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITenant load(long tenantId) throws ServerException;

	/**
	 * Changes the password of the administrator of the given tenant
	 * 
	 * @param password the new password for the administrator user
	 * @param tenantName name of the tenant
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeAdminPassword(String password, String tenantName) throws ServerException;

	/**
	 * Change session tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the selected tenant
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITenant changeSessionTenant(long tenantId) throws ServerException;

	/**
	 * Encodes the uploaded branding image
	 * 
	 * @return the Base64 image used for the brand
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String encodeBrandingImage() throws ServerException;

	/**
	 * Imports the uploaded branding package
	 * 
	 * @return the branding representation
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIBranding importBrandingPackage() throws ServerException;

	public static class Instance {
		private static TenantServiceAsync inst;

		private Instance() {
		}
		
		public static TenantServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(TenantService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}