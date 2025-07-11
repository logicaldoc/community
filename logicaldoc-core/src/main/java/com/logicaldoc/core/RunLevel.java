package com.logicaldoc.core;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.exception.AspectDisabledException;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

/**
 * Represents an operative status of the whole application. Different aspects
 * are enabled depending on the current runlevel.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public enum RunLevel {
	DEFAULT("default"), BULKLOAD("bulkload"), DEVEL("devel"), DEMO("demo"), UPDATED("updated"), SLAVE("slave");

	private static final Logger log = LoggerFactory.getLogger(RunLevel.class);

	private String level;

	RunLevel(String level) {
		this.level = level;
	}

	@Override
	public String toString() {
		return this.level;
	}

	public static RunLevel current() {
		ContextProperties config = getConfig();
		String runLevel = config != null ? config.getProperty("runlevel", DEFAULT.toString()) : DEFAULT.toString();
		return RunLevel.fromString(runLevel);
	}

	public static RunLevel fromString(String event) {
		if (event != null)
			for (RunLevel b : RunLevel.values())
				if (event.equalsIgnoreCase(b.level)) {
					return b;
				}
		return DEFAULT;
	}

	private String getAspectProperty(String aspect) {
		return "aspect." + aspect + "." + level;
	}

	public boolean aspectEnabled(String aspect) {
		ContextProperties config = getConfig();
		return config != null && config.getBoolean(getAspectProperty(aspect), false);
	}

	public void checkAspect(String aspect) throws AspectDisabledException {
		if(!aspectEnabled(aspect))
			throw new AspectDisabledException(aspect);
	}

	
	public void setAspect(String aspect, boolean enabled) {
		ContextProperties config = getConfig();
		if (config != null)
			config.setProperty(getAspectProperty(aspect), "" + enabled);
	}

	private static ContextProperties getConfig() {
		ContextProperties conf = null;
		try {
			Context context = Context.get();
			if (context != null)
				conf = context.getProperties();
		} catch (Exception t) {
			// Nothing to do
		}

		if (conf == null)
			try {
				conf = new ContextProperties();
			} catch (IOException e) {
				log.error(e.getMessage(), e);
			}
		return conf;
	}

	public boolean isDefault() {
		return this == DEFAULT;
	}

	public boolean isDemo() {
		return this == DEMO;
	}

	public boolean isBulkload() {
		return this == BULKLOAD;
	}

	public boolean isDevel() {
		return this == DEVEL;
	}

	public static List<String> getAspects() {
		// Acquire the 'Aspect' extensions
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Aspect");

		List<String> aspects = new ArrayList<>();
		for (Extension ext : exts)
			aspects.add(ext.getParameter("code").valueAsString());
		Collections.sort(aspects);

		return aspects;
	}
}