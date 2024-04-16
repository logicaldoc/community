package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.text.StrSubstitutor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

/**
 * A configuration utility used to retrieve and alter context properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class ContextProperties extends OrderedProperties {

	private static final String UNABLE_TO_READ_FROM = "Unable to read from %s";

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
			load(ContextProperties.class.getClassLoader().getResource("context.properties"));
		} catch (Exception e) {
			load(ContextProperties.class.getClassLoader().getResource("/context.properties"));
		}
	}

	public ContextProperties(String filePath) throws IOException {
		this.file = new File(filePath);

		// If the file does not exist, interpet it as a classpath reference
		if (!this.file.exists()) {
			try {
				if (filePath.startsWith("/"))
					filePath = filePath.substring(1);
				URL url = ContextProperties.class.getClassLoader().getResource(filePath);
				if ("file".equals(url.getProtocol()))
					this.file = new File(url.getPath());
			} catch (Exception e) {
				log.error("Unable to find classpath resource {}", filePath, e);
			}
		}

		try (FileInputStream fis = new FileInputStream(this.file)) {
			load(fis);
		} catch (IOException e) {
			throw new IOException(String.format(UNABLE_TO_READ_FROM, filePath), e);
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try (FileInputStream fis = new FileInputStream(this.overrideFile)) {
				load(fis);
				log.info("Override settings defined in {}", overrideFile.getPath());
			} catch (IOException e) {
				throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
			}
		}
	}

	private File detectOverrideFile() {
		if (file == null || !file.exists())
			return null;
		File override = new File(file.getParentFile(),
				FileUtil.getBaseName(file.getName()) + "-override." + FileUtil.getExtension(file.getName()));
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
		} catch (Exception e) {
			throw new IOException(String.format(UNABLE_TO_READ_FROM, file.getPath()), e);
		}
		try (FileInputStream fis = new FileInputStream(file)) {
			load(fis);
		} catch (IOException e) {
			throw new IOException(String.format(UNABLE_TO_READ_FROM, file.getPath()), e);
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try (FileInputStream fis = new FileInputStream(overrideFile)) {
				load(fis);
				log.debug("Override settings defined in {}", overrideFile.getPath());
			} catch (IOException e) {
				throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
			}
		}
	}

	public ContextProperties(File file) throws IOException {
		try {
			if (file.exists()) {
				this.file = file;
				try (FileInputStream fis = new FileInputStream(file)) {
					load(fis);
				}
			}
		} catch (IOException e) {
			throw new IOException("Unable to read from " + file.getPath(), e);
		}

		overrideFile = detectOverrideFile();
		if (overrideFile != null) {
			try (FileInputStream fis = new FileInputStream(overrideFile)) {
				load(fis);
			} catch (IOException e) {
				throw new IOException(String.format(UNABLE_TO_READ_FROM, overrideFile.getPath()), e);
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
			throw new IOException("Unable to read from stream", e);
		}
	}

	/**
	 * This method saves the properties-file connected by ContextProperties.<br>
	 * <b>NOTE:</b> only call this on an ContextProperties _NOT_ created from an
	 * InputStream!
	 * 
	 * @throws IOException raised when the file cannot be written
	 */
	public synchronized void write() throws IOException {
		checkFile();
		backup();

		File tmpFile = new File(file.getParentFile(), file.getName() + ".tmp");

		try {
			Files.deleteIfExists(tmpFile.toPath());
			Files.createFile(tmpFile.toPath());

			try (FileOutputStream fos = new FileOutputStream(tmpFile);) {
				store(fos, "");
				log.info("Saved settings into temp file {}", tmpFile.getAbsolutePath());
			}

			FileUtil.moveQuitely(tmpFile, file);
		} finally {
			FileUtil.strongDelete(tmpFile);
		}
	}

	private void checkFile() throws IOException {
		// it might be that we do not have an ordinary file,
		// so we can't write to it
		if (file == null)
			throw new IOException("File not given");
	}

	/**
	 * Makes a daily backup of the actual file. Up to <code>maxBackups</code>
	 * are maintained.
	 */
	protected void backup() throws IOException {
		if (maxBackups < 1)
			return;

		checkFile();

		// Backup the file first
		final File parent = file.getParentFile();

		/*
		 * Save the daily backup
		 */
		SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");
		String today = df.format(new Date());
		File backup = new File(parent, file.getName() + "." + today);
		if (!backup.exists()) {
			FileUtils.copyFile(file, backup);
			log.debug("Backup saved in {}", backup.getPath());
		}

		/*
		 * Delete the oldest backups
		 */
		deleteOldestBackups();
	}

	private void deleteOldestBackups() throws IOException {
		List<File> oldBackups = getBackups();
		if (oldBackups.size() > maxBackups) {
			List<File> backupsToRetain = oldBackups.stream().limit(maxBackups).toList();
			for (File backupFile : oldBackups)
				if (!backupsToRetain.contains(backupFile))
					FileUtil.strongDelete(backupFile);
		}
	}

	public List<File> getBackups() throws IOException {
		checkFile();

		File[] oldBackups = file.getParentFile().listFiles((dir, name) -> name.startsWith(file.getName() + ".")
				&& name.substring(name.lastIndexOf('.') + 1).length() == 8);

		// Sort old backup by descending date
		Arrays.sort(oldBackups, (f1, f2) -> {
			String date1 = f1.getName().substring(f1.getName().lastIndexOf('.') + 1);
			String date2 = f2.getName().substring(f2.getName().lastIndexOf('.') + 1);
			return date2.compareTo(date1);
		});

		return Arrays.asList(oldBackups);
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

	public double getDouble(String property) {
		return getDouble(property, 0D);
	}

	public double getDouble(String property, double defaultValue) {
		String v = getProperty(property);
		if (v == null || v.trim().isEmpty())
			return defaultValue;
		else
			return Double.parseDouble(v.trim());
	}

	public float getFloat(String property) {
		return getFloat(property, 0F);
	}

	public float getFloat(String property, float defaultValue) {
		String v = getProperty(property);
		if (v == null || v.trim().isEmpty())
			return defaultValue;
		else
			return Float.parseFloat(v.trim());
	}

	/**
	 * Gets the property value replacing all variable references
	 * 
	 * @param property name of the setting to process
	 * 
	 * @return the porperty's value with expanded variables
	 */
	@Override
	public String getProperty(String property) {
		return StrSubstitutor.replaceSystemProperties(super.getProperty(property));
	}

	@Override
	public String getProperty(String property, String defaultValue) {
		return StrSubstitutor.replaceSystemProperties(super.getProperty(property, defaultValue));
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
		Map<String, String> props = new HashMap<>();
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
		List<String> toBeDeleted = new ArrayList<>();
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