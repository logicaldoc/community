package com.logicaldoc.core.store;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This class is responsible for storers. All storers declared in the Storer
 * extension point.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class StorerManager {
	protected static Logger log = LoggerFactory.getLogger(StorerManager.class);

	// Key is the type, value is the associated builder
	private Map<String, Storer> storers = new HashMap<String, Storer>();

	private ContextProperties config;

	public static StorerManager get() {
		return (StorerManager) Context.get().getBean(StorerManager.class);
	}

	private StorerManager() {
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public synchronized void init() {
		if (!storers.isEmpty())
			return;

		// Acquire the 'Storer' extensions
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Storer");

		for (Extension ext : exts) {
			String type = ext.getParameter("type").valueAsString();
			String className = ext.getParameter("class").valueAsString();
			try {
				@SuppressWarnings("rawtypes")
				Class clazz = Class.forName(className);
				// Try to instantiate the builder
				Object storer = clazz.getDeclaredConstructor().newInstance();
				if (!(storer instanceof Storer))
					throw new Exception(
							String.format("The specified storer %s doesn't implement the Storer interface", className));
				storers.put(type, (Storer) storer);

				Storer st = (Storer) storer;
				for (String name : st.getParameterNames())
					st.getParameters().put(name, null);

				log.info("Added new storer {} for type {}", clazz.getSimpleName(), type);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	public Storer newStorer(int id) {
		if (storers.isEmpty())
			init();

		String type = config.getProperty("store." + id + ".type", "fs");
		Storer definition = getDefinitions().get(type);
		if (definition == null) {
			log.error("Unexisting definition for {}", type);
			return null;
		}

		Storer storer = null;
		try {
			storer = definition.getClass().getDeclaredConstructor().newInstance();
		} catch (Throwable e) {
			log.error("Unable to instanciate class {} / {}", definition.getClass(), e.getMessage(), e);
			return null;
		}
		storer.setId(id);

		Set<String> params = definition.getParameters().keySet();
		for (String param : params)
			storer.getParameters().put(param, config.getPropertyWithSubstitutions("store." + id + "." + param, ""));

		return storer;
	}

	public Map<String, Storer> getDefinitions() {
		if (storers.isEmpty())
			init();
		return storers;
	}
}