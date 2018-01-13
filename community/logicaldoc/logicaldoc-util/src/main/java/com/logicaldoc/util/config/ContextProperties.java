package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.text.StrSubstitutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A configuration utility used to retrieve and alter context properties
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.0
 */
public class ContextProperties extends OrderedProperties {

	private static final long serialVersionUID = 1L;

	/** this points to an ordinary file */
	private String docPath;

	protected static Logger log = LoggerFactory.getLogger(ContextProperties.class);

	protected int maxBackups = 1;

	public ContextProperties(int maxBackups) throws IOException {
		this();
		this.maxBackups = maxBackups;
	}

	public ContextProperties() throws IOException {
		try {
			load(ContextProperties.class.getClassLoader().getResource("/context.properties"));
		} catch (Throwable t) {
			try {
				load(ContextProperties.class.getClassLoader().getResource("context.properties"));
			} catch (Throwable q) {
				throw new IOException(q.getMessage(), q);
			}
		}
	}

	public ContextProperties(String docname) throws IOException {
		docPath = docname;
		try {
			load(new FileInputStream(docPath));
		} catch (IOException e) {
			log.error("Unable to read from " + docPath, e);
			throw e;
		}
	}

	public ContextProperties(URL docname) throws IOException {
		load(docname);
	}

	/**
	 * Loads the file from the given URL
	 */
	private void load(URL docname) throws IOException {
		try {
			docPath = URLDecoder.decode(docname.getPath(), "UTF-8");
		} catch (IOException e) {
			log.error("Unable to read from " + docPath, e);
			throw e;
		}
		try {
			load(new FileInputStream(docPath));
		} catch (IOException e) {
			log.error("Unable to read from " + docPath, e);
			throw e;
		}
	}

	public ContextProperties(File doc) throws IOException {
		try {
			docPath = doc.getPath();
			if (doc.exists())
				load(new FileInputStream(docPath));
		} catch (IOException e) {
			log.error("Unable to read from " + docPath, e);
			throw e;
		}
	}

	/**
	 * Creates new XMLBean from an input stream; XMLBean is read-only!!!
	 */
	public ContextProperties(InputStream is) throws IOException {
		docPath = null;
		try {
			load(is);
		} catch (IOException e) {
			log.error("Unable to read from stream");
			throw e;
		}
	}

	/**
	 * This method saves the properties-file connected by ContextProperties.<br>
	 * <b>NOTE:</b> only call this on an ContextProperties _NOT_ created from an
	 * InputStream!
	 * 
	 * @throws IOException
	 */
	public void write() throws IOException {
		// it might be that we do not have an ordinary file,
		// so we can't write to it
		if (docPath == null)
			throw new IOException("Path not given");

		backup();

		FileOutputStream fos = new FileOutputStream(docPath);
		try {
			store(fos, "");
			log.info("Saved file " + docPath);
		} catch (IOException ex) {
			if (log.isWarnEnabled()) {
				log.warn(ex.getMessage());
			}
			throw ex;
		} finally {
			if (fos != null)
				try {
					fos.flush();
					fos.close();
				} catch (Throwable t) {
				}
		}
	}

	/**
	 * makes a backup of the actual file. Up to <code>maxBackups</code> are
	 * maintained
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	protected void backup() throws IOException {
		// Backup the file first
		final File src = new File(docPath);
		final File parent = src.getParentFile();
		File backup = null;

		// Check if there is a free slot between 1 and maxBackups
		for (int i = 1; i <= maxBackups; i++) {
			File test = new File(parent + "/" + src.getName() + "." + i);
			if (!test.exists()) {
				backup = test;
				break;
			}
		}

		// No free slots, we have to replace an existing one
		if (backup == null) {
			File[] oldBackups = parent.listFiles(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.startsWith(src.getName() + ".");
				}
			});

			// Sort old backup by date
			Arrays.sort(oldBackups, new Comparator() {
				public int compare(Object o1, Object o2) {

					if (((File) o1).lastModified() < ((File) o2).lastModified()) {
						return -1;
					} else if (((File) o1).lastModified() > ((File) o2).lastModified()) {
						return +1;
					} else {
						return 0;
					}
				}
			});

			// Take the oldest one, so we will reuse it
			backup = oldBackups[0];
		}

		FileUtils.copyFile(src, backup);
		log.debug("Backup saved in " + backup.getPath());
	}

	public int getInt(String property) {
		return getInt(property, 0);
	}

	public int getInt(String property, int defaultValue) {
		String v = getProperty(property);
		if (v == null || v.isEmpty())
			return defaultValue;
		else
			return Integer.parseInt(getProperty(property));
	}

	public boolean getBoolean(String property) {
		return getBoolean(property, false);
	}

	public boolean getBoolean(String property, boolean defaultValue) {
		String v = getProperty(property, "" + defaultValue);
		return "true".equals(v) || "yes".equals(v) || "1".equals(v);
	}

	/**
	 * Gets the property value replacing all variable references
	 */
	public String getPropertyWithSubstitutions(String property) {
		return StrSubstitutor.replaceSystemProperties(getProperty(property));
	}

	public String getPropertyWithSubstitutions(String property, String defaultValue) {
		return StrSubstitutor.replaceSystemProperties(getProperty(property, defaultValue));
	}

	public int getMaxBackups() {
		return maxBackups;
	}

	public void setMaxBackups(int maxBackups) {
		this.maxBackups = maxBackups;
	}

	public String getTenantProperty(String tenant, String property) {
		String key = tenant + "." + property;
		if (containsKey(key))
			return getProperty(key);
		else
			return getProperty(property);
	}

	public Map<String, String> getTenantProperties(String tenant) {
		return getProperties(tenant + ".");
	}

	/**
	 * Gets all the properties whose name starts with the given prefix.
	 */
	public Map<String, String> getProperties(String prefix) {
		Map<String, String> props = new HashMap<String, String>();
		for (Object key : keySet()) {
			String prop = key.toString();
			if (prop.startsWith(prefix))
				props.put(prop.substring(prefix.length()), getProperty(prop));
		}
		return props;
	}

	/**
	 * Removes all the properties of a specific tenant
	 */
	public void removeTenantProperties(String tenant) {
		if ("default".equals(tenant))
			return;
		List<String> toBeDeleted = new ArrayList<String>();
		for (Object key : keySet()) {
			String prop = key.toString();
			if (prop.startsWith(tenant + "."))
				toBeDeleted.add(prop);
		}
		for (String prop : toBeDeleted)
			remove(prop);
	}

	/**
	 * Replicates the settings of the default tenant to a new tenant.
	 */
	public void replicateTenantSettings(String tenant) {
		Map<String, String> defaultProps = getTenantProperties("default");
		for (String prop : defaultProps.keySet()) {
			String tenantProp = tenant + "." + prop;
			if (!containsKey(tenantProp))
				setProperty(tenantProp, getProperty("default." + prop));
		}
	}
}