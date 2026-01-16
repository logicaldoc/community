package com.logicaldoc.core.document;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.util.plugin.PluginRegistry;

import jakarta.annotation.PostConstruct;

/**
 * A manager for document listeners. It's internals are initialized from the
 * extension point 'DocumentListener' of the core plugin.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Component("documentListenerManager")
public class DocumentListenerManager {

	private static final String POSITION = "position";

	private static final Logger log = LoggerFactory.getLogger(DocumentListenerManager.class);

	private List<DocumentListener> listeners = new ArrayList<>();

	@PostConstruct
	public synchronized void init() {
		if (!listeners.isEmpty())
			return;

		// Acquire the 'DocumentListener' extensions of the core plugin sorted
		// by position
		for (Extension ext : PluginRegistry.getInstance().getExtensions("logicaldoc-core", "DocumentListener").stream()
				.sorted((e1, e2) -> Integer.valueOf(e1.getParameter("position").valueAsString())
						.compareTo(Integer.valueOf(e2.getParameter("position").valueAsString())))
				.toList()) {
			String className = ext.getParameter("class").valueAsString();

			try {
				Class<?> clazz = Class.forName(className);
				// Try to instantiate the listener
				Object listener = clazz.getDeclaredConstructor().newInstance();
				if (listener instanceof DocumentListener l) {
					listeners.add(l);
					if (log.isInfoEnabled())
						log.info("Added document listener {} position {}", className,
								ext.getParameter(POSITION).valueAsString());
				} else {
					throw new ClassNotFoundException(
							"The specified listener %s doesn't implement DocumentListener interface"
									.formatted(className));
				}
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
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
