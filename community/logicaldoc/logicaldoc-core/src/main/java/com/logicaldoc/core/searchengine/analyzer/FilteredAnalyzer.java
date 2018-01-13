package com.logicaldoc.core.searchengine.analyzer;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.AnalyzerWrapper;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.util.TokenFilterFactory;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.searchengine.StandardSearchEngine;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This analyzer is a wrapper to be used to handle an ordered list of filters.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class FilteredAnalyzer extends AnalyzerWrapper {

	protected static Logger log = LoggerFactory.getLogger(FilteredAnalyzer.class);

	public static final ThreadLocal<String> lang = new ThreadLocal<String>() {
		@Override
		protected String initialValue() {
			return "en";
		}
	};

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
		Tokenizer tokenizer = components.getTokenizer();

		Map<String, String> filters = getTokenFilters();
		List<String> order = getTokenFilterNames(true);

		ContextProperties config = Context.get().getProperties();

		/*
		 * Iterate over the configured filters and progressively create a new
		 * TokenStream over ts
		 */
		for (String filter : order) {

			// Prepare the configuration of the filter
			Map<String, String> configs = new HashMap<String, String>();
			configs.put("luceneMatchVersion", StandardSearchEngine.VERSION.toString());
			configs.put("lang", lang.get());
			configs.putAll(config.getProperties("index.tokenfilter." + filter + "."));

			String filterClass = filters.get(filter);
			@SuppressWarnings("rawtypes")
			Class aClass = null;
			try {
				aClass = Class.forName(filterClass);
			} catch (Throwable t) {
				log.error(filterClass + " not found");
			}

			try {
				@SuppressWarnings({ "rawtypes", "unchecked" })
				Constructor constructor = aClass.getConstructor(new Class[] { java.util.Map.class });
				if (constructor != null) {
					TokenFilterFactory factory = (TokenFilterFactory) constructor.newInstance(configs);
					ts = factory.create(ts);
				}
				log.debug("Appended token stream filter " + filterClass);
			} catch (Throwable e) {
				log.debug("constructor (Map<String, String>) not found for " + filterClass);
			}
		}

		return new TokenStreamComponents(tokenizer, ts);
	}

	private static Map<String, String> getTokenFilters() {
		Map<String, String> activeAnalyzers = new HashMap<String, String>();

		// Acquire the 'TokenFilter' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		if (registry == null)
			return activeAnalyzers;

		Collection<Extension> extensions = new ArrayList<Extension>();
		try {
			extensions = registry.getExtensions("logicaldoc-core", "TokenFilter");
		} catch (Throwable e) {
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
	 */
	public static List<String> getTokenFilterNames(boolean justActives) {
		ContextProperties config = Context.get().getProperties();
		Map<String, String> filters = getTokenFilters();
		ArrayList<String> names = new ArrayList<String>();
		if (!justActives)
			names.addAll(filters.keySet());
		else {
			for (String name : filters.keySet()) {
				if ("enabled".equals(config.getProperty("index.tokenfilter." + name)))
					names.add(name);
			}
		}
		names.sort(new Comparator<String>() {

			@Override
			public int compare(String name1, String name2) {
				Integer pos1 = config.getInt("index.tokenfilter." + name1 + ".position", 1);
				Integer pos2 = config.getInt("index.tokenfilter." + name2 + ".position", 1);
				return pos1.compareTo(pos2);
			}

		});

		return names;
	}
}