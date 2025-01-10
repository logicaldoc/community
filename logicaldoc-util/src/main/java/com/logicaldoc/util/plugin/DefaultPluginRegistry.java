package com.logicaldoc.util.plugin;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.java.plugin.Plugin;
import org.java.plugin.PluginLifecycleException;
import org.java.plugin.registry.Identity;
import org.java.plugin.registry.PluginDescriptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic implementation of a <code>PluginRegistry</code>
 * 
 * @author Marco Meschieri
 *
 * @since 3.0
 */
public class DefaultPluginRegistry extends PluginRegistry {

	protected void initPlugins(Map<String, Identity> plugins) {
		Logger console = LoggerFactory.getLogger("console");
		console.info("Intialising plugins");
		Set<String> keys = plugins.keySet();
		Iterator<String> iterator = keys.iterator();
		while (iterator.hasNext()) {
			String manifest = iterator.next();
			console.info("manifestURL: {}", manifest);

			PluginDescriptor pluginDescriptor = (PluginDescriptor) plugins.get(manifest);
			console.info("plugin located: {} @ {}", pluginDescriptor.getId(), pluginDescriptor.getLocation());

			try {
				Plugin plugin = manager.getPlugin(pluginDescriptor.getId());

				console.info("Intialising plugin: {}", plugin.getDescriptor());
				console.info("plugin located: {}", plugin.getDescriptor().getLocation());

				manager.activatePlugin(plugin.getDescriptor().getId());
				console.info("Activated plugin {}", plugin.getDescriptor().getId());
			} catch (SecurityException e) {
				console.error("PluginRegistry -> SecurityException: {}", e.getMessage());
			} catch (IllegalArgumentException e) {
				console.error("PluginRegistry -> IllegalArgumentException: {}", e.getMessage());
			} catch (PluginLifecycleException e) {
				console.error("PluginRegistry -> PluginLifecycleException: {}", e.getMessage());
			} catch (Exception e) {
				console.error("PluginRegistry -> Error: {}", e.getMessage());
			}
		}
	}

}
