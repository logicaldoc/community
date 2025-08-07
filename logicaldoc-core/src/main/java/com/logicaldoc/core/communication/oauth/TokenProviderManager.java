package com.logicaldoc.core.communication.oauth;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.util.plugin.PluginRegistry;

import jakarta.annotation.PostConstruct;

/**
 * A manager for the {@link TokenProvider}s. It's internals are initialized from
 * the extension point 'TokenProvider' of the core plugin.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
@Component("tokenProviderManager")
public class TokenProviderManager {

	private static final Logger log = LoggerFactory.getLogger(TokenProviderManager.class);

	/**
	 * Map of providers, key is the protocol of the provider (microsoft, google
	 * ...)
	 */
	private Map<String, TokenProvider> providers = new HashMap<>();

	@PostConstruct
	public synchronized void init() {
		providers.clear();

		// Acquire the 'TokenProvider' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "TokenProvider");
		for (Extension ext : exts) {
			String className = ext.getParameter("class").valueAsString();
			String protocols = ext.getParameter("protocols").valueAsString().replace(" ", "");

			try {
				Class<?> clazz = Class.forName(className);
				// Try to instantiate the listener
				Object provider = clazz.getDeclaredConstructor().newInstance();
				if (provider instanceof TokenProvider tp) {
					for (String protocol : List.of(protocols.split(","))) {
						providers.put(protocol, tp);
						if (log.isInfoEnabled())
							log.info("Registered token provider {} for protocol {}", className, protocol);
					}
				} else {
					throw new ClassNotFoundException(
							"The specified provider " + className + " doesn't implement TokenProvider interface");
				}

			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
				log.error(e.getMessage());
			}
		}

		initDefault();
	}

	private void initDefault() {
		if (!providers.containsKey("smtpmicrosoft365"))
			providers.put("smtpmicrosoft365", new Microsoft365TokenProvider());
		if (!providers.containsKey("imapmicrosoft365"))
			providers.put("imapmicrosoft365", new Microsoft365TokenProvider());
	}

	public TokenProvider getProvider(String key) {
		initDefault();
		return providers.get(key);
	}
}