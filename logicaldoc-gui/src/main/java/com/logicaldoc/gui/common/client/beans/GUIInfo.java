package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.i18n.I18N;

/**
 * General product informations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIInfo implements Serializable {

	private static final long serialVersionUID = 1L;

	private String release = "8.9";

	private String year = "2006-2023";

	private String userNo;

	private String installationId;

	private String licensee;

	private int sessionHeartbeat = -1;

	private String runLevel;

	private String hostName;

	private Date date = new Date();

	private GUITenant tenant = null;

	// Optional list of messages to be shown to the user
	private GUIMessage[] alerts = new GUIMessage[0];

	private GUIValue[] supportedLanguages = new GUIValue[0];

	private GUIValue[] supportedGUILanguages = new GUIValue[0];

	private GUIValue[] bundle = new GUIValue[0];

	private GUIValue[] config = new GUIValue[0];
	
	private GUIReadingRequest[] unconfirmedReagings = new GUIReadingRequest[0];

	// The definitions of attributes
	private GUIAttribute[] attributeDefinitions = new GUIAttribute[0];

	private String[] features = new String[0];

	private boolean databaseConnected = true;

	private GUIAttributeSet defaultAttributeSet;

	private GUIBranding branding = new GUIBranding();
	
	public String getRelease() {
		return release;
	}

	public void setRelease(String release) {
		this.release = release;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public GUIMessage[] getAlerts() {
		return alerts;
	}

	public void setAlerts(GUIMessage[] alerts) {
		this.alerts = alerts;
	}

	public GUIValue[] getSupportedLanguages() {
		return supportedLanguages;
	}

	public void setSupportedLanguages(GUIValue[] supportedLanguages) {
		this.supportedLanguages = supportedLanguages;
	}

	public GUIValue[] getBundle() {
		return bundle;
	}

	public void setBundle(GUIValue[] bundle) {
		this.bundle = bundle;
	}

	public String[] getFeatures() {
		return features;
	}

	public boolean isEnabled(String feature) {
		if (features == null || features.length == 0)
			return false;
		else {
			for (String f : features) {
				if (f.equals(feature))
					return true;
			}
		}
		return false;
	}

	public void setFeatures(String[] features) {
		this.features = features;
	}

	public String getInstallationId() {
		return installationId;
	}

	public void setInstallationId(String installationId) {
		this.installationId = installationId;
	}

	public GUIValue[] getSupportedGUILanguages() {
		return supportedGUILanguages;
	}

	public void setSupportedGUILanguages(GUIValue[] supportedGUILanguages) {
		this.supportedGUILanguages = supportedGUILanguages;
	}

	public String getLicensee() {
		return licensee;
	}

	public void setLicensee(String licensee) {
		this.licensee = licensee;
	}

	public int getSessionHeartbeat() {
		return sessionHeartbeat;
	}

	public void setSessionHeartbeat(int sessionHeartbeat) {
		this.sessionHeartbeat = sessionHeartbeat;
	}

	public GUIValue[] getConfig() {
		return config;
	}

	/**
	 * Search for the given configuration property. this is the lookup logic:
	 * <ol>
	 * <li><b>current_tenant</b>.name</li>
	 * <li>name</li>
	 * </ol>
	 * 
	 * @param name Name of the property
	 * @param defaultValue value to return in case the property is undefined
	 * 
	 * @return The value
	 */
	public String getConfig(String name, String defaultValue) {
		String tenantName = Constants.TENANT_DEFAULTNAME;
		if (tenant != null)
			tenantName = tenant.getName();

		/*
		 * Look for tenant specific
		 */
		for (GUIValue val : getConfig()) {
			if ((tenantName + "." + name).equals(val.getCode()))
				return val.getValue();
		}

		/*
		 * Look for general property
		 */
		for (GUIValue val : getConfig()) {
			if (name.equals(val.getCode()))
				return val.getValue();
		}

		return defaultValue;
	}

	/**
	 * Search for the given configuration property. this is the lookup logic:
	 * <ol>
	 * <li><b>current_tenant</b>.name</li>
	 * <li>name</li>
	 * </ol>
	 * 
	 * @param name Name of the property
	 * @return The value
	 */
	public String getConfig(String name) {
		return getConfig(name, null);
	}

	public void setConfig(String name, String value) {
		for (GUIValue val : getConfig()) {
			if (name.equals(val.getCode())) {
				val.setValue(value);
				return;
			}
		}
	}

	public void setConfig(GUIValue[] config) {
		this.config = config;
	}

	public String getRunLevel() {
		return runLevel;
	}

	public void setRunLevel(String runLevel) {
		this.runLevel = runLevel;
	}

	public boolean isDatabaseConnected() {
		return databaseConnected;
	}

	public void setDatabaseConnected(boolean databaseConnected) {
		this.databaseConnected = databaseConnected;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public GUITenant getTenant() {
		return tenant;
	}

	public void setTenant(GUITenant tenant) {
		this.tenant = tenant;
	}

	public String getUserNo() {
		return userNo;
	}

	public void setUserNo(String userNo) {
		this.userNo = userNo;
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String hostName) {
		this.hostName = hostName;
	}

	public GUIAttributeSet getDefaultAttributeSet() {
		return defaultAttributeSet;
	}

	public void setDefaultAttributeSet(GUIAttributeSet defaultAttributeSet) {
		this.defaultAttributeSet = defaultAttributeSet;
	}

	public GUIAttribute getAttributeDefinition(String name) {
		String n = name;
		if (n.startsWith("ext_"))
			n = name.substring(4);
		for (GUIAttribute val : attributeDefinitions) {
			if (val.getName().equals(n))
				return val;
		}
		return null;
	}

	public String getAttributeLabel(String name) {
		String n = name;
		if (n.startsWith("ext_"))
			n = name.substring(4);
		for (GUIAttribute val : attributeDefinitions) {
			if (val.getName().equals(n))
				return val.getLabel() != null && !val.getLabel().isEmpty() ? val.getLabel() : val.getName();
		}
		return I18N.message(n);
	}

	public GUIBranding getBranding() {
		return branding;
	}

	public void setBranding(GUIBranding branding) {
		this.branding = branding;
	}

	public GUIAttribute[] getAttributeDefinitions() {
		return attributeDefinitions;
	}

	public void setAttributeDefinitions(GUIAttribute[] attributeDefinitions) {
		this.attributeDefinitions = attributeDefinitions;
	}

	public GUIReadingRequest[] getUnconfirmedReagings() {
		return unconfirmedReagings;
	}

	public void setUnconfirmedReagings(GUIReadingRequest[] unconfirmedReagings) {
		this.unconfirmedReagings = unconfirmedReagings;
	}
}