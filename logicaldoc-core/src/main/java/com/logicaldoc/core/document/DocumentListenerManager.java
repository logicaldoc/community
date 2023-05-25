package com.logicaldoc.core.document;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * A manager for document listeners. It's internals are initialized from the
 * extension point 'DocumentListener' of the core plugin.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class DocumentListenerManager {

	private static final String POSITION = "position";

	protected static Logger log = LoggerFactory.getLogger(DocumentListenerManager.class);

	private List<DocumentListener> listeners = new ArrayList<>();

	@SuppressWarnings("rawtypes")
	public synchronized void init() {
		if (!listeners.isEmpty())
			return;

		// Acquire the 'DocumentListener' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "DocumentListener");

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
				if (!(listener instanceof DocumentListener))
					throw new ClassNotFoundException(
							"The specified listener " + className + " doesn't implement DocumentListener interface");
				listeners.add((DocumentListener) listener);
				log.info("Added new document listener {} position {}", className,
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
	public List<DocumentListener> getListeners() {
		if (listeners.isEmpty())
			init();
		return listeners;
	}
}
