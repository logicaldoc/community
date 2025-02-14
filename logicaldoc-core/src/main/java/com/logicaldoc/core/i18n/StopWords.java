package com.logicaldoc.core.i18n;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.ResourceUtil;

/**
 * Class for retrieving the words from the stopwords files
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public abstract class StopWords {

	private static final Logger log = LoggerFactory.getLogger(StopWords.class);

	private static final Map<Locale, Set<String>> cacheMap = new HashMap<>();
	
	private StopWords() {
		// do nothing
	}

	public static Set<String> getStopWords(String locale) {
		return cacheMap.computeIfAbsent(LocaleUtil.toLocale(locale), StopWords::loadStopWords);
	}

	public static Set<String> getStopWords(Locale locale) {
		return cacheMap.computeIfAbsent(locale, StopWords::loadStopWords);
	}

	private static Set<String> loadStopWords(Locale locale) {
		String resource = "/stopwords/stopwords_" + locale + ".txt";
		if (!ResourceUtil.existsResource(resource))
			resource = "/stopwords/stopwords_" + locale.getLanguage() + ".txt";
		if (!ResourceUtil.existsResource(resource))
			resource = "/stopwords/stopwords_en.txt";
		log.debug("Loading stopwords from: {}", resource);
		try (BufferedReader reader = new BufferedReader(new StringReader(ResourceUtil.readAsString(resource)));) {
			String line = "";
			Set<String> words = new HashSet<>();
			while ((line = reader.readLine()) != null)
				words.add(line.trim());
			return words;
		} catch (IOException e) {
			log.warn(e.getMessage(), e);
			return Collections.emptySet();
		}
	}
}
