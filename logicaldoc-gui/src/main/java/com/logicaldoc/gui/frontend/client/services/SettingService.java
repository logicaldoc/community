package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * The client side stub for the Settings Service. This service allows the
 * management of various application settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("setting")
public interface SettingService extends RemoteService {

	/**
	 * Loads web services, webDav and other protocol settings
	 * 
	 * @return the protocol settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadProtocolSettings() throws ServerException;

	/**
	 * Loads the complete settings set
	 * 
	 * @return all the settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadSettings() throws ServerException;

	/**
	 * Loads a set of settings values
	 * 
	 * @param names The setting names to be retrieved
	 * 
	 * @return The array of settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadSettingsByNames(List<String> names) throws ServerException;

	/**
	 * Saves the registration settings
	 * 
	 * @param name person name
	 * @param email email contact
	 * @param organization organization name
	 * @param website the corporate website
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveRegistration(String name, String email, String organization, String website) throws ServerException;

	/**
	 * Saves settings
	 * 
	 * @param settings the settings to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveSettings(List<GUIParameter> settings) throws ServerException;

	/**
	 * Loads email settings (SMTP connection)
	 * 
	 * @return the mail server settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIEmailSettings loadEmailSettings() throws ServerException;

	/**
	 * Tests the SMTP connection
	 * 
	 * @param email email address to test(it will receive a test message)
	 * 
	 * @return True only if the email was sent
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean testEmail(String email) throws ServerException;

	/**
	 * Tests a store (read/write access)
	 * 
	 * @param id identifier of the store to test
	 * 
	 * @return True only if the store has read/write permission
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean testStore(int id) throws ServerException;

	/**
	 * Saves settings related to the store
	 * 
	 * @param settings the store settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveStoreSettings(List<GUIParameter> settings) throws ServerException;

	/**
	 * Saves settings related to the firewall
	 * 
	 * @param settings the firewall settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveFirewallSettings(List<GUIParameter> settings) throws ServerException;

	/**
	 * Tries to delete a store and fails in case at least one folder is using
	 * it and also if the store is marked as the default write one
	 * 
	 * @param storeId identifier of the store to remove
	 * 
	 * @return list of paths using the store
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<String> removeStore(int storeId) throws ServerException;

	/**
	 * Saves email settings (SMTP connection)
	 * 
	 * @param settings the SMTP settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveEmailSettings(GUIEmailSettings settings) throws ServerException;

	/**
	 * Load the GUI settings
	 * 
	 * @return the User Interface settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadGUISettings() throws ServerException;

	/**
	 * Loads the parameters of a specified converter
	 * 
	 * @param converter name of the converter
	 * 
	 * @return the configuration parameters of the converter
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadConverterParameters(String converter) throws ServerException;

	/**
	 * Loads the usage stats of the webservice
	 * 
	 * @param tenantId identifier of the tenant to consider
	 * 
	 * @return all the stats
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIParameter> loadWebserviceStats(Long tenantId) throws ServerException;

	/**
	 * Persists new aliases for the given extension
	 * 
	 * @param extension the main file extension
	 * @param aliases comma-separated set of aliases
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveExtensionAliases(String extension, String aliases) throws ServerException;

	public static class Instance {
		private static SettingServiceAsync inst;

		private Instance() {
		}
		
		public static SettingServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SettingService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}