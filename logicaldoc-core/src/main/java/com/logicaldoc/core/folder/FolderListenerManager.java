package com.logicaldoc.core.folder;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.annotation.PostConstruct;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * A manager for folder listeners. It's internals are initialized from the
 * extension point 'FolderListener' of the core plugin.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
@Component("folderListenerManager")
public class FolderListenerManager {

	private static final String POSITION = "position";

	protected static Logger log = LoggerFactory.getLogger(FolderListenerManager.class);

	private List<FolderListener> listeners = new ArrayList<>();

	@PostConstruct
	@SuppressWarnings("rawtypes")
	public synchronized void init() {
		if (!listeners.isEmpty())
			return;

		// Acquire the 'FolderListener' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "FolderListener");

		// Sort the extensions according to ascending position
		List<Extension> sortedExts = new ArrayList<>();
		for (Extension extension : exts) {
			sortedExts.add(extension);
		}
		Collections.sort(sortedExts, (Extension e1, Extension e2) -> {
			int position1 = Integer.parseInt(e1.getParameter(POSITION).valueAsString());
			int position2 = Integer.parseInt(e2.getParameter(POSITION).valueAsString());
			if (position1 < position2)
				return -1;
			else if (position1 > position2)
				return 1;
			else
				return 0;
		});

		for (Extension ext : sortedExts) {
			String className = ext.getParameter("class").valueAsString();

			try {
				Class clazz = Class.forName(className);
				// Try to instantiate the listener
				@SuppressWarnings("unchecked")
				Object listener = clazz.getDeclaredConstructor().newInstance();
				if (!(listener instanceof FolderListener))
					throw new ClassNotFoundException(
							"The specified listener " + className + " doesn't implement FolderListener interface");
				listeners.add((FolderListener) listener);
				if (log.isInfoEnabled())
					log.info("Added new folder listener {} position {}", className,
							ext.getParameter(POSITION).valueAsString());
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException | SecurityException e) {
				log.error(e.getMessage());
			}

		}
	}

	/**
	 * The ordered list of listeners
	 * 
	 * @return the list of listeners
	 */
	public List<FolderListener> getListeners() {
		if (listeners.isEmpty())
			init();
		return listeners;
	}
}
