package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.beans.GUITenant;

/**
 * The client side stub for the Tenant Service. This service allows r/w
 * operations on tenants.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.9
 */
@RemoteServiceRelativePath("tenant")
public interface TenantService extends RemoteService {
	/**
	 * Deletes a specific tenant by its ID
	 */
	public void delete(long tenantId) throws ServerException;

	/**
	 * Saves/Updates a given tenant
	 */
	public GUITenant save(GUITenant tenant) throws ServerException;

	/**
	 * Loads a given tenant
	 */
	public GUITenant load(long tenantId) throws ServerException;

	/**
	 * Changes the password of the administrator of the given tenant
	 */
	public void changeAdminPassword(String password, String tenantName) throws ServerException;

	/**
	 * Change session tenant
	 */
	public GUITenant changeSessionTenant(long tenantId) throws ServerException;

	/**
	 * Encodes the uploaded branding image
	 */
	public String encodeBrandingImage() throws ServerException;

	/**
	 * Imports the uploaded branding package
	 */
	public GUIBranding importBrandingPackage() throws ServerException;

	public static class Instance {
		private static TenantServiceAsync instance;

		public static TenantServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(TenantService.class);
			}
			return instance;
		}
	}
}