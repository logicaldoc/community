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

public class SystemInfo extends AbstractSystemInfo {
	protected static Logger log = LoggerFactory.getLogger(SystemInfo.class);

	protected static final long serialVersionUID = 1L;

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
				try {
					@SuppressWarnings("rawtypes")
					Class clazz = Class.forName(className);
					// Try to instantiate the info
					@SuppressWarnings("unchecked")
					Object tmp = clazz.getDeclaredConstructor().newInstance();
					if (!(tmp instanceof SystemInfo))
						throw new Exception(
								"The specified info " + className + " doesn't implement SystemInfo interface");

					info = (SystemInfo) tmp;
					info.setTenantId(tenantId);
				} catch (Throwable e) {
					log.error(e.getMessage(), e);
				}
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
}