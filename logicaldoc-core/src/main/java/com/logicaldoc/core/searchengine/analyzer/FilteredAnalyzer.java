package com.logicaldoc.core.searchengine.analyzer;

import java.io.File;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.AnalyzerWrapper;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.miscellaneous.WordDelimiterGraphFilterFactory;
import org.apache.lucene.analysis.util.FilesystemResourceLoader;
import org.apache.lucene.analysis.util.ResourceLoader;
import org.apache.lucene.analysis.util.TokenFilterFactory;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.searchengine.StandardSearchEngine;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

/**
 * This analyzer is a wrapper to be used to handle an ordered list of filters.
 * Please see also
 * https://lucene.apache.org/solr/guide/8_5/understanding-analyzers-tokenizers-and-filters.html
 * 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class FilteredAnalyzer extends AnalyzerWrapper {

	private static final String INDEX_TOKENFILTER = "index.tokenfilter.";

	private static final Logger log = LoggerFactory.getLogger(FilteredAnalyzer.class);

	public static final ThreadLocal<String> lang = ThreadLocal.withInitial(() -> "en");

	public FilteredAnalyzer() {
		super(GLOBAL_REUSE_STRATEGY);
	}

	@Override
	protected Analyzer getWrappedAnalyzer(String fieldName) {
		return getLanguage().getAnalyzer();
	}

	private Language getLanguage() {
		LanguageManager man = LanguageManager.getInstance();
		Language language = man.getLanguage(LocaleUtil.toLocale(lang.get()));
		if (language == null)
			language = new Language(Locale.ENGLISH);
		return language;
	}

	@Override
	protected TokenStreamComponents wrapComponents(String fieldName, TokenStreamComponents components) {
		TokenStream ts = components.getTokenStream();

		Map<String, String> filters = getTokenFilters();
		List<String> order = getTokenFilterNames(true);
		ContextProperties config = Context.get().getConfig();

		/*
		 * Iterate over the configured filters and progressively create a new
		 * TokenStream over ts
		 */
		for (String filter : order) {
			// Prepare the configuration of the filter
			Map<String, String> configs = new HashMap<>();
			configs.put("luceneMatchVersion", StandardSearchEngine.VERSION.toString());
			configs.put("lang", lang.get());

			// Add the specific settings
			Map<String, String> filterSettings = config.getProperties(INDEX_TOKENFILTER + filter + ".");
			for (Map.Entry<String, String> entry : filterSettings.entrySet()) {
				String val = entry.getValue();
				if (StringUtils.isNotEmpty(val))
					configs.put(entry.getKey(), val);
			}

			String filterClass = filters.get(filter);

			if (!filterClass.equalsIgnoreCase(SnowballFilterFactory.class.getName())) {
				/**
				 * Remove unrecognized settings that will cause exception during
				 * construction
				 */
				configs.remove("lang");
				configs.remove("position");
			}

			@SuppressWarnings("rawtypes")
			Class aClass = null;
			try {
				aClass = Class.forName(filterClass);
			} catch (Exception t) {
				log.error("{} not found", filterClass);
			}

			ts = intantiateFilterClass(aClass, ts, configs);
		}

		return new TokenStreamComponents(components.getSource(), ts);
	}

	private TokenStream intantiateFilterClass(@SuppressWarnings("rawtypes")
	Class aClass, TokenStream ts, Map<String, String> configs) {
		if (aClass != null) {
			try {
				@SuppressWarnings({ "rawtypes", "unchecked" })
				Constructor constructor = aClass.getConstructor(java.util.Map.class);

				TokenFilterFactory factory = (TokenFilterFactory) constructor.newInstance(configs);

				if (factory instanceof WordDelimiterGraphFilterFactory wdFactory) {
					/**
					 * This class may need initialization from files
					 */
					ResourceLoader loader = new FilesystemResourceLoader(
							new File(Context.get().getConfig().getProperty("index.dir") + "/logicaldoc/conf")
									.toPath(),
							this.getClass().getClassLoader());
					wdFactory.inform(loader);
				}

				ts = factory.create(ts);

				log.debug("Appended token stream filter {}", aClass.getName());
			} catch (NoSuchMethodException nse) {
				log.warn("constructor (Map<String, String>) not found for {}", aClass.getName());
			} catch (Exception e) {
				log.warn("constructor (Map<String, String>) of {} raised an error: {}", aClass.getName(),
						e.getMessage(), e);
			}
		}
		return ts;
	}

	private static Map<String, String> getTokenFilters() {
		Map<String, String> activeAnalyzers = new HashMap<>();

		// Acquire the 'TokenFilter' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		if (registry == null)
			return activeAnalyzers;

		Collection<Extension> extensions = new ArrayList<>();
		try {
			extensions = registry.getExtensions("logicaldoc-core", "TokenFilter");
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		for (Extension ext : extensions) {
			String name = ext.getParameter("name").valueAsString();
			String clazz = ext.getParameter("factory").valueAsString();
			activeAnalyzers.put(name, clazz);
		}

		return activeAnalyzers;
	}

	/**
	 * Ordered list of token filter names
	 * 
	 * @param justActives if we have to consider just the active tokens
	 * 
	 * @return list of token names
	 */
	public static List<String> getTokenFilterNames(boolean justActives) {
		ContextProperties config = Context.get().getConfig();
		Map<String, String> filters = getTokenFilters();
		ArrayList<String> names = new ArrayList<>();
		if (!justActives)
			names.addAll(filters.keySet());
		else {
			for (String name : filters.keySet()) {
				if ("enabled".equals(config.getProperty(INDEX_TOKENFILTER + name)))
					names.add(name);
			}
		}
		names.sort((String name1, String name2) -> {
			Integer pos1 = config.getInt(INDEX_TOKENFILTER + name1 + ".position", 1);
			Integer pos2 = config.getInt(INDEX_TOKENFILTER + name2 + ".position", 1);
			return pos1.compareTo(pos2);
		});

		return names;
	}
}