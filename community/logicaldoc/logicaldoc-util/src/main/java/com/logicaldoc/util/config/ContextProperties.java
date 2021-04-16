package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.text.StrSubstitutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A configuration utility used to retrieve and alter context properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class ContextProperties extends OrderedProperties {

	private static final long serialVersionUID = 1L;

	/** this points to an ordinary file */
	private File file;

	private File overrideFile;

	protected static Logger log = LoggerFactory.getLogger(ContextProperties.class);

	protected int maxBackups = 10;

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

	public ContextProperties(String filePath) throws IOException {
		this.file = new File(filePath);
		try {
			load(new FileInputStream(this.file));
		} catch (Throwable e) {
			log.error("Unable to read from {}", filePath, e);
			throw e;
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try {
				load(new FileInputStream(overrideFile));
				log.info("Override settings defined in {}", overrideFile.getPath());
			} catch (Throwable e) {
				log.error("Unable to read from {}", overrideFile.getPath(), e);
				throw e;
			}
		}
	}

	private File detectOverrideFile() {
		if (file == null || !file.exists())
			return null;
		File override = new File(file.getParentFile(),
				FilenameUtils.getBaseName(file.getName()) + "-override." + FilenameUtils.getExtension(file.getName()));
		return override.exists() ? override : null;
	}

	public ContextProperties(URL fileUrl) throws IOException {
		load(fileUrl);
	}

	/**
	 * Loads the file from the given URL
	 */
	private void load(URL fileUrl) throws IOException {
		try {
			file = new File(URLDecoder.decode(fileUrl.getPath(), "UTF-8"));
		} catch (IOException e) {
			log.error("Unable to read from {}", file, e);
			throw e;
		}
		try {
			load(new FileInputStream(file));
		} catch (IOException e) {
			log.error("Unable to read from {}", file, e);
			throw e;
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try {
				load(new FileInputStream(overrideFile));
				log.debug("Override settings defined in {}", overrideFile.getPath());
			} catch (Throwable e) {
				log.error("Unable to read from {}", overrideFile.getPath(), e);
				throw e;
			}
		}
	}

	public ContextProperties(File file) throws IOException {
		try {
			if (file.exists()) {
				this.file = file;
				load(new FileInputStream(file));
			}
		} catch (IOException e) {
			log.error("Unable to read from " + file.getPath(), e);
			throw e;
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try {
				load(new FileInputStream(overrideFile));
			} catch (Throwable e) {
				log.error("Unable to read from {}", overrideFile.getPath(), e);
				throw e;
			}
		}
	}

	/**
	 * Creates new XMLBean from an input stream; XMLBean is read-only!!!
	 *
	 * @param is the stream that represents the XML to parse
	 * 
	 * @throws IOException raised when the stream cannot be read
	 */
	public ContextProperties(InputStream is) throws IOException {
		file = null;
		overrideFile = null;
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
	 * @throws IOException raised when the file cannot be written
	 */
	public void write() throws IOException {
		// it might be that we do not have an ordinary file,
		// so we can't write to it
		if (file == null)
			throw new IOException("File not given");
		backup(file);

		FileOutputStream fos = new FileOutputStream(file);
		try {
			store(fos, "");
			log.info("Saved file {}", file);
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
	 * Makes a daily backup of the actual file. Up to <code>maxBackups</code>
	 * are maintained.
	 * 
	 * @param src the source file to backup
	 */
	protected void backup(File src) throws IOException {
		// Backup the file first
		final File parent = src.getParentFile();

		/*
		 * Save the daily backup
		 */
		SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");
		String today = df.format(new Date());
		File backup = new File(parent, src.getName() + "." + today);
		if (!backup.exists()) {
			FileUtils.copyFile(src, backup);
			log.debug("Backup saved in {}", backup.getPath());
		}

		/*
		 * Delete the oldest backups
		 */
		File[] oldBackups = parent.listFiles(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith(src.getName() + ".") && name.substring(name.lastIndexOf('.') + 1).length() == 8;
			}
		});

		// Sort old backup by descending date
		Arrays.sort(oldBackups, new Comparator<File>() {
			public int compare(File f1, File f2) {
				String date1 = f1.getName().substring(f1.getName().lastIndexOf('.') + 1);
				String date2 = f2.getName().substring(f2.getName().lastIndexOf('.') + 1);
				return date2.compareTo(date1);
			}
		});

		if (oldBackups.length > maxBackups)
			for (int i = maxBackups - 1; i < oldBackups.length; i++)
				oldBackups[i].delete();
	}

	public String getString(String property) {
		return getProperty(property);
	}

	public String getString(String property, String defaultValue) {
		return getProperty(property, defaultValue);
	}

	public int getInt(String property) {
		return getInt(property, 0);
	}

	public int getInt(String property, int defaultValue) {
		String v = getProperty(property);
		if (v == null || v.trim().isEmpty())
			return defaultValue;
		else
			return Integer.parseInt(v.trim());
	}

	public long getLong(String property) {
		return getLong(property, 0);
	}

	public long getLong(String property, long defaultValue) {
		String v = getProperty(property);
		if (v == null || v.trim().isEmpty())
			return defaultValue;
		else
			return Long.parseLong(v.trim());
	}

	public boolean getBoolean(String property) {
		return getBoolean(property, false);
	}

	public boolean getBoolean(String property, boolean defaultValue) {
		String v = getProperty(property, "" + defaultValue).trim();
		return "true".equals(v) || "yes".equals(v) || "1".equals(v);
	}

	/**
	 * Gets the property value replacing all variable references
	 * 
	 * @param property name of the setting to process
	 * 
	 * @return the porperty's value with expanded variables
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
	 * 
	 * @param prefix property's prefix
	 * 
	 * @return the map property_name = property_value
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
	 * 
	 * @param tenant name of the tenant
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
	 * Replicates the settings of the default tenant to a new tenant
	 * 
	 * @param tenant name of the tenant
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