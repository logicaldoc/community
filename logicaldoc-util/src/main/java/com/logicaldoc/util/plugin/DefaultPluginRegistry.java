package com.logicaldoc.util.plugin;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.java.plugin.Plugin;
import org.java.plugin.PluginLifecycleException;
import org.java.plugin.registry.Identity;
import org.java.plugin.registry.PluginDescriptor;

/**
 * Basic implementation of a <code>PluginRegistry</code>
 * 
 * @author Marco Meschieri
 *
 * @since 3.0
 */
public class DefaultPluginRegistry extends PluginRegistry {

	protected void initPlugins(Map<String, Identity> plugins) {
		System.out.println("Intialising plugins");
		Set<String> keys = plugins.keySet();
		Iterator<String> iterator = keys.iterator();
		while (iterator.hasNext()) {
			String manifest = iterator.next();
			System.out.println("manifestURL: " + manifest);

			PluginDescriptor pluginDescriptor = (PluginDescriptor) plugins.get(manifest);
			System.out.println("plugin located: " + pluginDescriptor.getId() + " @ " + pluginDescriptor.getLocation());

			try {
				Plugin plugin = manager.getPlugin(pluginDescriptor.getId());

				System.out.println("Intialising plugin: " + plugin.getDescriptor());
				System.out.println("plugin located: " + plugin.getDescriptor().getLocation());

				manager.activatePlugin(plugin.getDescriptor().getId());
				System.out.println("Activated plugin " + plugin.getDescriptor().getId());
			} catch (SecurityException e) {
				System.err.println("PluginRegistry -> SecurityException: " + e.getMessage());
			} catch (IllegalArgumentException e) {
				System.err.println("PluginRegistry -> IllegalArgumentException: " + e.getMessage());
			} catch (PluginLifecycleException e) {
				System.err.println("PluginRegistry -> IllegalArgumentException: " + e.getMessage());
			} catch (Exception e) {
				System.err.println("PluginRegistry -> Error: " + e.getMessage());
			}
		}
	}

}
