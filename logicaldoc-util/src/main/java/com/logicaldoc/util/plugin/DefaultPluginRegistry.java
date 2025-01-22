package com.logicaldoc.util.plugin;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.java.plugin.Plugin;
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
		console.info("Intializing plugins");
		Set<String> keys = plugins.keySet();
		Iterator<String> iterator = keys.iterator();
		while (iterator.hasNext()) {
			String manifest = iterator.next();
			console.info("manifestURL: {}", manifest);

			PluginDescriptor pluginDescriptor = (PluginDescriptor) plugins.get(manifest);
			console.info("plugin located: {} @ {}", pluginDescriptor.getId(), pluginDescriptor.getLocation());

			try {
				console.info("Intializing plugin: {} located in {}", pluginDescriptor.getId(),
						pluginDescriptor.getLocation());

				manager.activatePlugin(pluginDescriptor.getId());

				Plugin plugin = manager.getPlugin(pluginDescriptor.getId());
				console.info("Activated plugin {}", plugin.getDescriptor().getId());
			} catch (Exception e) {
				console.error(e.getMessage(), e);
			}
		}
	}

}
