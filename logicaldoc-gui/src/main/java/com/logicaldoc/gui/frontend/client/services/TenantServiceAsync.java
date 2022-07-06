package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.beans.GUITenant;

public interface TenantServiceAsync {
	void delete(long tenantId, AsyncCallback<Void> callback);

	void save(GUITenant tenant, AsyncCallback<GUITenant> callback);

	void load(long tenantId, AsyncCallback<GUITenant> callback);

	void changeAdminPassword(String password, String tenantName, AsyncCallback<Void> callback);

	void changeSessionTenant(long tenantId, AsyncCallback<GUITenant> callback);

	void encodeBrandingImage(AsyncCallback<String> callback);

	void importBrandingPackage(AsyncCallback<GUIBranding> callback);
}