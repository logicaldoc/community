package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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

	private String release = "9.1";

	private String year = "2006-2025";

	private String userNo;

	private String installationId;

	private String licensee;

	private int sessionHeartbeat = -1;

	private String runLevel;

	private String hostName;

	private String changelog;

	private Date date = new Date();

	private GUITenant tenant = null;

	// Optional list of messages to be shown to the user
	private List<GUIMessage> alerts = new ArrayList<>();

	private List<GUIValue> supportedLanguages = new ArrayList<>();

	private List<GUIValue> supportedGUILanguages = new ArrayList<>();

	private List<GUIValue> bundle = new ArrayList<>();

	private List<GUIValue> config = new ArrayList<>();

	private List<GUIReadingRequest> unconfirmedReagings = new ArrayList<>();

	// The definitions of attributes
	private List<GUIAttribute> attributeDefinitions = new ArrayList<>();

	private List<String> features = new ArrayList<>();

	private boolean databaseConnected = true;

	private GUIAttributeSet defaultAttributeSet;

	private GUIBranding branding = new GUIBranding();

	public String getRelease() {
		return release;
	}

	public String getChangelog() {
		return changelog;
	}

	public void setChangelog(String changelog) {
		this.changelog = changelog;
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

	public List<GUIMessage> getAlerts() {
		return alerts;
	}

	public void setAlerts(List<GUIMessage> alerts) {
		this.alerts = alerts;
	}

	public List<GUIValue> getSupportedLanguages() {
		return supportedLanguages;
	}

	public void setSupportedLanguages(List<GUIValue> supportedLanguages) {
		this.supportedLanguages = supportedLanguages;
	}

	public List<GUIValue> getBundle() {
		return bundle;
	}

	public void setBundle(List<GUIValue> bundle) {
		this.bundle = bundle;
	}

	public List<String> getFeatures() {
		return features;
	}

	public boolean isEnabled(String feature) {
		for (String f : features) {
			if (f.equals(feature))
				return true;
		}
		return false;
	}

	public void setFeatures(List<String> features) {
		this.features = features;
	}

	public String getInstallationId() {
		return installationId;
	}

	public void setInstallationId(String installationId) {
		this.installationId = installationId;
	}

	public List<GUIValue> getSupportedGUILanguages() {
		return supportedGUILanguages;
	}

	public void setSupportedGUILanguages(List<GUIValue> supportedGUILanguages) {
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

	public List<GUIValue> getConfig() {
		return config;
	}

	public void setConfig(List<GUIValue> config) {
		this.config = config;
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

	public List<GUIAttribute> getAttributeDefinitions() {
		return attributeDefinitions;
	}

	public void setAttributeDefinitions(List<GUIAttribute> attributeDefinitions) {
		this.attributeDefinitions = attributeDefinitions;
	}

	public List<GUIReadingRequest> getUnconfirmedReagings() {
		return unconfirmedReagings;
	}

	public void setUnconfirmedReagings(List<GUIReadingRequest> unconfirmedReagings) {
		this.unconfirmedReagings = unconfirmedReagings;
	}
}