package com.logicaldoc.util.plugin;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Properties;

import org.java.plugin.Plugin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;

/**
 * This is the base class for LogicalDOC plugins.
 * 
 * The methods of this class are automatically invoked by the manager during
 * plug-in life cycle management (activation and deactivation).
 * 
 * In addition, this class makes available methods to manage the plug-in's
 * properties stored in file plugin.properties, and allows access to the plug-in
 * framework (see org.java.plugin.PluginManager manager), {see
 * org.java.plugin.registry.PluginRegistry registry) which it was loaded.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public abstract class LogicalDOCPlugin extends Plugin {

	protected static Logger log = LoggerFactory.getLogger(LogicalDOCPlugin.class);

	private static final String PLUGIN_PROPERTIES = "plugin.properties";

	private Properties data = new Properties();

	/**
	 * This method will be called once during plug-in activation before any
	 * access to any code from this plug-in. If the plug-in needs to be
	 * installed(@link #isInstallNeeded())the <code>install()</code> method is
	 * called
	 * 
	 * @throws Exception if an error has occurred during plug-in start-up
	 * @see org.java.plugin.Plugin#doStart()
	 */
	@Override
	protected void doStart() throws Exception {
		loadData();
		try {
			if (isInstallNeeded()) {
				install();
				getInstallMark().createNewFile();
				log.info("Plugin " + getDescriptor().getId() + " installed");
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
		start();
		saveData();
	}

	/**
	 * Return the install mark file, that is the file called 'install' inside
	 * the plug-in's data folder
	 */
	private File getInstallMark() {
		String id = null;
		try {
			ContextProperties conf = new ContextProperties();
			id = conf.getProperty("id");
		} catch (IOException e) {
		}
		return new File(resolveDataPath("install-" + id).toString());
	}

	/**
	 * Tells is the plug-in need to be installed. This default implementation
	 * check if the file 'install' exists o the plug-in's data folder. Concrete
	 * implementations are free to re-implement this logic.
	 * 
	 * @return true if the plug-in need to be reinstalled
	 */
	protected boolean isInstallNeeded() {
		return !getInstallMark().exists();
	}

	protected void loadData() throws IOException {
		try (FileInputStream fis = new FileInputStream(resolveDataFile())) {
			data.load(fis);
		}
	}

	protected void saveData() throws IOException {
		try (FileOutputStream fos = new FileOutputStream(resolveDataFile())) {
			data.store(fos, "");
		}
	}

	/**
	 * Retrieves the path where the plugins jar archives are stored
	 * 
	 * @return the path where the plugins jar archives are stored
	 */
	public String getPluginPath() {
		String path = getManager().getPathResolver().resolvePath(getDescriptor(), "/").toString();
		if (path.startsWith("jar:file:"))
			path = path.substring("jar:file:".length());
		path = path.substring(0, path.lastIndexOf("!"));
		try {
			// The path may contain URL-encoded sequences
			path = URLDecoder.decode(path, "UTF-8");
		} catch (UnsupportedEncodingException e) {
		}
		return path;
	}

	/**
	 * Resolves a relative path inside the plugin data folder
	 * 
	 * @param relativePath The relative path
	 * @return The file
	 */
	public File resolveDataPath(String relativePath) {
		return new File(getDataDirectory(), relativePath);
	}

	private File resolveDataFile() {
		File file = new File(getDataDirectory(), PLUGIN_PROPERTIES);
		if (!file.exists())
			try {
				file.createNewFile();
			} catch (IOException e) {
				log.error(e.getMessage());
			}
		return file;
	}

	/**
	 * Returns the data directory for this plugin, that is
	 * {conf.plugindir}/{pluginName}. It will be created in not existing.
	 * 
	 * @return the folder for the data of the plugin
	 */
	public File getDataDirectory() {
		String pluginName = getPluginName();
		return PluginRegistry.getPluginHome(pluginName);
	}

	protected String getPluginName() {
		return getDescriptor().getUniqueId().substring(0, getDescriptor().getUniqueId().lastIndexOf('@'));
	}

	/**
	 * Concrete implementations of this method must insert first installation
	 * logic such as database initialization.
	 * 
	 * @throws Exception raised if some errors happened during the installation
	 */
	public void install() throws Exception {
	}

	/**
	 * Concrete implementations of this method must insert startup
	 * initializations.
	 * 
	 * @throws Exception raised if some errors happened during the startup
	 */
	protected void start() throws Exception {
	}

	protected void setRestartRequired() {
		if (getDescriptor() != null)
			System.out.println("Plugin " + getDescriptor().getId() + " requires a restart");
		PluginRegistry.getInstance().setRestartRequired();
	}

	/**
	 * This method will be called once during plug-in deactivation. After this
	 * method call, no other code from this plug-in can be accessed, unless
	 * {@link #doStart()} method will be called again (but for another instance
	 * of this class).
	 * 
	 * @throws Exception if an error has occurred during plug-in shutdown
	 * @see org.java.plugin.Plugin#doStop()
	 */
	@Override
	protected void doStop() throws Exception {
	}

	public Properties getData() {
		return data;
	}

	public String getProperty(String name) {
		return data.getProperty(name);
	}
}