package com.logicaldoc.util.plugin;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.JpfException;
import org.java.plugin.ObjectFactory;
import org.java.plugin.PluginManager;
import org.java.plugin.registry.Extension;
import org.java.plugin.registry.ExtensionPoint;
import org.java.plugin.registry.Identity;
import org.java.plugin.registry.PluginDescriptor;
import org.java.plugin.util.ExtendedProperties;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.http.UrlUtil;
import com.logicaldoc.util.io.FileUtil;

/**
 * Central point where plugins are loaded and handled. The class is abstract and
 * must be personalized as needed. The used implementation can be specified with
 * the 'logicaldoc.app.pluginregistry' system property.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public abstract class PluginRegistry {

	protected PluginManager manager = null;

	private static PluginRegistry instance;

	private boolean restartRequired = false;

	public static PluginRegistry getInstance() {
		if (instance == null) {
			String pluginregistry = null;
			try {
				ContextProperties config = new ContextProperties();
				pluginregistry = config.getProperty("plugin.registry");
			} catch (IOException e1) {
				System.err.println(e1.getMessage());
			}

			if (StringUtils.isEmpty(pluginregistry)) {
				pluginregistry = DefaultPluginRegistry.class.getName();
			}
			try {
				System.out.println("Instantiate concrete PluginRegistry: " + pluginregistry);
				instance = (PluginRegistry) Class.forName(pluginregistry).getDeclaredConstructor().newInstance();
			} catch (Exception e) {
				System.err.println(e.getMessage());
			}
		}
		return instance;
	}

	/**
	 * Initializes all found plugins
	 * 
	 * @param pluginsDir the root folder where the plugins files are located
	 * 
	 * @throws PluginException Error trying to publish the plugins
	 */
	public void init(String pluginsDir) throws PluginException {
		ExtendedProperties properties = new ExtendedProperties();
		properties.put("com.logicaldoc.util.ShadingPathResolver.unpackMode", "smart");

		ObjectFactory pluginObjectFactory = ObjectFactory.newInstance(properties);
		manager = pluginObjectFactory.createManager();

		List<PluginManager.PluginLocation> pluginLocations;
		try {
			pluginLocations = locatePlugins(pluginsDir);
		} catch (Exception e) {
			throw new PluginException("Error locating plugins", e);
		}

		if (!pluginLocations.isEmpty()) {
			Map<String, Identity> plugins = null;
			try {
				PluginManager.PluginLocation[] pLocations = pluginLocations
						.toArray(new PluginManager.PluginLocation[0]);
				plugins = manager.publishPlugins(pLocations);
			} catch (JpfException e) {
				throw new PluginException("Error publishing plugins", e);
			}

			System.out.println("Succesfully registered " + plugins.size() + " plugins");
			initPlugins(plugins);
		}
	}

	/**
	 * Initializes found plugins
	 * 
	 * @param plugins Map of found plugins
	 * 
	 * @throws IOException I/O error
	 */
	protected abstract void initPlugins(Map<String, Identity> plugins);

	protected List<PluginManager.PluginLocation> locatePlugins(String pluginsDirectoryPath) throws IOException {

		List<PluginManager.PluginLocation> pluginLocations = new LinkedList<>();

		// look for all zip files in plugin directory
		File pluginDirectory = new File(pluginsDirectoryPath);

		System.out.println("Searching for plugins in " + pluginDirectory.getAbsolutePath());

		FilenameFilter filter = (dir, fileName) -> (fileName.endsWith(".jar") && fileName.contains("-plugin"));

		if (!pluginDirectory.isDirectory()) {
			System.out.println("No Plugins Found");
		}

		// find the plugins
		List<String> pluginsList = Arrays.asList(pluginDirectory.list(filter));

		if (!pluginsList.isEmpty()) {
			Iterator<String> i = pluginsList.iterator();

			while (i.hasNext()) {
				String pluginFilename = i.next();

				File pluginZIPFile = new File(pluginDirectory.getPath() + "/" + pluginFilename);

				if (!pluginZIPFile.exists())
					throw new IOException("file not Found:" + pluginZIPFile.getAbsolutePath());

				try {
					final URL manifestURL = UrlUtil
							.toURL("jar:file:" + pluginZIPFile.getAbsolutePath() + "!/plugin.xml");
					final URL contextURL = pluginZIPFile.toURI().toURL();

					System.out.println("Found plugin file: " + pluginZIPFile.getName());

					pluginLocations.add(new PluginManager.PluginLocation() {
						public URL getManifestLocation() {
							return manifestURL;
						}

						public URL getContextLocation() {
							return contextURL;
						}
					});
				} catch (MalformedURLException | URISyntaxException e) {
					System.err.println(e.getMessage());
				}
			}

		} else {
			throw new IOException("Unable to access Plugins directory: " + pluginDirectory.getAbsolutePath());
		}
		return pluginLocations;
	}

	public PluginManager getManager() {
		return manager;
	}

	/**
	 * Returns the extensions connected to the specified extension point
	 * 
	 * @param pluginId The plugin identifier
	 * @param extensionPoint The extension point id
	 * @return List of connected extensions
	 */
	public Collection<Extension> getExtensions(String pluginId, String extensionPoint) {
		Collection<Extension> exts = new ArrayList<>();
		try {
			PluginRegistry registry = PluginRegistry.getInstance();
			if (registry != null) {
				PluginDescriptor descriptor = registry.getManager().getRegistry().getPluginDescriptor(pluginId);
				ExtensionPoint dbinitExtPoint = registry.getManager().getRegistry()
						.getExtensionPoint(descriptor.getId(), extensionPoint);
				exts = dbinitExtPoint.getConnectedExtensions();
			}
		} catch (Exception e) {
			// Nothing to do
		}
		return exts;
	}

	/**
	 * Returns the extensions connected to the specified extension point
	 * 
	 * @param pluginId The plugin identifier
	 * @param extensionPoint The extension point id
	 * @param sortingParameter Extensions will be sorted by this parameter (if
	 *        null 'position' parameter is used)
	 * @return List of connected extensions
	 */
	public List<Extension> getSortedExtensions(String pluginId, String extensionPoint, final String sortingParameter) {
		Collection<Extension> exts = getExtensions(pluginId, extensionPoint);

		// Sort the extensions according to ascending position
		List<Extension> sortedExts = new ArrayList<>();
		for (Extension extension : exts) {
			sortedExts.add(extension);
		}

		Collections.sort(sortedExts, (e1, e2) -> {
			String sortParam = "position";
			if (StringUtils.isNotEmpty(sortingParameter))
				sortParam = sortingParameter;
			int position1 = Integer.parseInt(e1.getParameter(sortParam).valueAsString());
			int position2 = Integer.parseInt(e2.getParameter(sortParam).valueAsString());
			if (position1 < position2)
				return -1;
			else if (position1 > position2)
				return 1;
			else
				return 0;
		});
		return sortedExts;
	}

	/**
	 * Retrieves the list of registered plugins
	 * 
	 * @return The list of registered plugins descriptors
	 */
	public Collection<PluginDescriptor> getPlugins() {
		PluginRegistry registry = PluginRegistry.getInstance();
		if (registry != null)
			return registry.getManager().getRegistry().getPluginDescriptors();
		else
			return new ArrayList<>();
	}

	/**
	 * Retrieve the plugin descriptor
	 * 
	 * @param pluginId identifier of the plugin
	 * 
	 * @return The plugin descriptor
	 */
	public PluginDescriptor getPlugin(String pluginId) {
		PluginRegistry registry = PluginRegistry.getInstance();
		if (registry != null)
			return registry.getManager().getRegistry().getPluginDescriptor(pluginId);
		else
			return null;
	}

	public boolean isRestartRequired() {
		return restartRequired;
	}

	public void setRestartRequired() {
		this.restartRequired = true;
	}

	/**
	 * This method retrieves the folder of the given plugin. If not exists, it
	 * creates the folder. The folder is:
	 * <b>conf.plugindir</b>/<b>pluginName</b>
	 * 
	 * @param pluginName The plugin name
	 * 
	 * @return the directory used as plugin's home
	 */
	public static File getPluginHome(String pluginName) {
		File root = getPluginsDir();
		File userDir = new File(root != null ? root : new File(""), pluginName);

		if (!FileUtil.isInsideFolder(root, userDir))
			throw new SecurityException(String.format("Plugin %s non inside plugin home %s", pluginName,
					root != null ? root.getPath() : ""));

		if (!userDir.exists()) {
			try {
				FileUtils.forceMkdir(userDir);
			} catch (IOException e) {
				return null;
			}
		}
		return userDir;
	}

	/**
	 * This method retrieves the plugins root folder. If not exists, it creates
	 * the folder. The folder is: <b>conf.plugindir</b>
	 * 
	 * @return root of the plugins data folders
	 */
	public static File getPluginsDir() {
		File pluginsPath = null;
		try {
			ContextProperties conf = new ContextProperties();
			pluginsPath = new File(conf.getProperty("conf.plugindir"));
		} catch (IOException e1) {
			// Nothing to do
		}

		try {
			FileUtils.forceMkdir(pluginsPath);
		} catch (IOException e) {
			return new File(".");
		}
		return pluginsPath;
	}

	/**
	 * This method retrieves a plugin folder resource (file or folder). If the
	 * resource is a folder and not exists, it creates the folder. The folder
	 * will be: <b>conf.plugindir</b>/<b>pluginName</b>/<b>path</b>
	 * 
	 * @param pluginName The plugin name
	 * @param path The resource path
	 * 
	 * @return the plugin's resource
	 */
	public static File getPluginResource(String pluginName, String path) {
		File root = getPluginHome(pluginName);
		File resource = new File((root != null ? root.getPath() : "") + "/" + path);

		if (!FileUtil.isInsideFolder(root, resource))
			throw new SecurityException(String.format("Resource %s non inside plugin home %s", resource.getPath(),
					root != null ? root.getPath() : ""));

		if (!resource.exists() && !path.contains("."))
			try {
				FileUtils.forceMkdir(resource);
			} catch (IOException e) {
				return null;
			}

		return resource;
	}
}