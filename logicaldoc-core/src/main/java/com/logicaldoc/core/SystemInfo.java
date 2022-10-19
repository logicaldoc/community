package com.logicaldoc.core;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * General informations about the system
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class SystemInfo {

	protected static Logger log = LoggerFactory.getLogger(SystemInfo.class);

	protected static final long serialVersionUID = 1L;

	protected String productName = "LogicalDOC Community";

	protected String product = "LogicalDOC";

	protected String release = "8.8.3";

	protected String year = "2006-2022";

	protected String help = "https://docs.logicaldoc.com";

	protected String bugs = "https://bugs.logicaldoc.com";

	protected String url = "https://www.logicaldoc.com";

	protected String forum = "https://forums.logicaldoc.com";

	protected String vendor = "LogicalDOC";

	protected String vendorAddress = "via Aldo Moro interna, 3";

	protected String vendorCap = "41012";

	protected String vendorCountry = "Italy";

	protected String vendorCity = "Carpi";

	protected String support = "support@logicaldoc.com";

	protected String installationId;

	protected String licensee;

	protected String runLevel;

	protected long tenantId = Tenant.DEFAULT_ID;

	protected Date date = new Date();

	protected SystemInfo() {
	}

	protected SystemInfo(long tenantId) {
		this.tenantId = tenantId;
	}

	public static SystemInfo get(long tenantId) {

		SystemInfo info = new SystemInfo(tenantId);

		/*
		 * Collect product identification
		 */
		try {
			// Acquire the 'SystemInfo' extensions of the core plugin
			PluginRegistry registry = PluginRegistry.getInstance();
			Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "SystemInfo");

			if (!exts.isEmpty()) {
				String className = exts.iterator().next().getParameter("class").valueAsString();

				@SuppressWarnings("rawtypes")
				Class clazz = Class.forName(className);

				// Try to instantiate the info
				@SuppressWarnings("unchecked")
				Object tmp = clazz.getDeclaredConstructor().newInstance();
				if (!(tmp instanceof SystemInfo))
					throw new Exception("The specified info " + className + " doesn't implement SystemInfo interface");

				info = (SystemInfo) tmp;
				info.setTenantId(tenantId);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		/*
		 * Collect installed features
		 */
		if (info.getFeatures() == null || info.getFeatures().length == 0)
			try {
				List<String> features = new ArrayList<String>();
				PluginRegistry registry = PluginRegistry.getInstance();
				Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Feature");
				for (Extension extension : exts) {
					// Retrieve the task name
					String name = extension.getParameter("name").valueAsString();
					if (!features.contains(name))
						features.add(name);
				}
				info.setFeatures(features.toArray(new String[0]));
			} catch (Throwable e) {
				log.error(e.getMessage());
			}

		/*
		 * Read some informations from the context
		 */
		try {
			// Read some informations from the context
			ContextProperties config = Context.get().getProperties();
			info.setRelease(config.getProperty("product.release"));
			info.setYear(config.getProperty("product.year"));
			info.setRunLevel(config.getProperty("runlevel"));
			info.setInstallationId(config.getProperty("id"));
		} catch (Throwable e) {
			log.error(e.getMessage());
		}

		// Get the host name
		try {
			info.setHostName(InetAddress.getLocalHost().getHostName());
		} catch (UnknownHostException e) {
			// Nothing to do
		}

		return info;
	}

	public static SystemInfo get() {
		return get(Tenant.DEFAULT_ID);
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public int getMajor() {
		String[] parts = getRelease().split("\\.");
		if (parts.length > 0)
			return Integer.parseInt(parts[0]);
		else
			return 0;
	}

	public int getMinor() {
		String[] parts = getRelease().split("\\.");
		if (parts.length > 1)
			return Integer.parseInt(parts[1]);
		else
			return 0;
	}

	public int getMicro() {
		String[] parts = getRelease().split("\\.");
		if (parts.length > 2)
			return Integer.parseInt(parts[2]);
		else
			return 0;
	}

	/**
	 * Deducted server's host name
	 */
	protected String hostName;

	protected String[] features;

	public String getProductName() {
		return productName;
	}

	public void setProductName(String productName) {
		this.productName = productName;
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	}

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

	public String getHelp() {
		return help;
	}

	public void setHelp(String help) {
		this.help = help;
	}

	public String getBugs() {
		return bugs;
	}

	public void setBugs(String bugs) {
		this.bugs = bugs;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getForum() {
		return forum;
	}

	public void setForum(String forum) {
		this.forum = forum;
	}

	public String getVendor() {
		return vendor;
	}

	public void setVendor(String vendor) {
		this.vendor = vendor;
	}

	public String getVendorAddress() {
		return vendorAddress;
	}

	public void setVendorAddress(String vendorAddress) {
		this.vendorAddress = vendorAddress;
	}

	public String getVendorCap() {
		return vendorCap;
	}

	public void setVendorCap(String vendorCap) {
		this.vendorCap = vendorCap;
	}

	public String getVendorCountry() {
		return vendorCountry;
	}

	public void setVendorCountry(String vendorCountry) {
		this.vendorCountry = vendorCountry;
	}

	public String getVendorCity() {
		return vendorCity;
	}

	public void setVendorCity(String vendorCity) {
		this.vendorCity = vendorCity;
	}

	public String getSupport() {
		return support;
	}

	public void setSupport(String support) {
		this.support = support;
	}

	public String getInstallationId() {
		return installationId;
	}

	public void setInstallationId(String installationId) {
		this.installationId = installationId;
	}

	public String getLicensee() {
		return licensee;
	}

	public void setLicensee(String licensee) {
		this.licensee = licensee;
	}

	public String getRunLevel() {
		return runLevel;
	}

	public void setRunLevel(String runLevel) {
		this.runLevel = runLevel;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String hostName) {
		this.hostName = hostName;
	}

	public String[] getFeatures() {
		return features;
	}

	public void setFeatures(String[] features) {
		this.features = features;
	}
}