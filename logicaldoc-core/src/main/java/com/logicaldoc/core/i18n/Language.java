package com.logicaldoc.core.i18n;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.SimpleAnalyzer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.tartarus.snowball.SnowballProgram;

import com.logicaldoc.core.searchengine.StandardSearchEngine;

/**
 * Instances of this class represent a language supported by the LogicalDOC DMS
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0.3
 */
public class Language implements Comparable<Language> {

	protected static Logger log = LoggerFactory.getLogger(Language.class);

	private Locale locale;

	private Set<String> stopWords = new HashSet<>();

	private Analyzer analyzer;

	private String analyzerClass;

	private SnowballProgram stemmer;

	public Language(Locale locale) {
		this.locale = locale;
		loadStopwords();
	}

	public Locale getLocale() {
		return locale;
	}

	public String getLanguage() {
		return locale.getLanguage();
	}

	public String getDisplayLanguage() {
		return locale.getDisplayLanguage();
	}

	public String getDefaultDisplayLanguage() {
		return locale.getDisplayLanguage(Locale.ENGLISH);
	}

	/**
	 * Populates the field stopWords reading the resource
	 * /stopwords/stopwords_<b>locale</b>.txt
	 */
	void loadStopwords() {
		try {
			Set<String> swSet = new HashSet<>();
			String stopwordsResource = "/stopwords/stopwords_" + getLocale().toString() + ".txt";
			log.debug("Loading stopwords from: {}", stopwordsResource);
			InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(stopwordsResource);
			if (is == null)
				is = getClass().getResourceAsStream(stopwordsResource);

			if (is == null) {
				log.warn("No stopwords found for locale {}", getLocale());
			} else {
				InputStreamReader isr = new InputStreamReader(is, StandardCharsets.UTF_8);
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				while ((line = br.readLine()) != null) {
					line = line.trim();
					if (line.indexOf("|") != -1) {
						line = line.substring(0, line.indexOf("|"));
						line = line.trim();
					}
					if (line != null && line.length() > 0 && !swSet.contains(line)) {
						swSet.add(line);
					}
				}
			}
			stopWords = swSet;
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}
	}

	public Set<String> getStopWords() {
		return stopWords;
	}

	public void setStopWords(Set<String> stopWords) {
		this.stopWords = stopWords;
	}

	public String getAnalyzerClass() {
		return analyzerClass;
	}

	public void setAnalyzerClass(String analyzerClass) {
		this.analyzerClass = analyzerClass;
	}

	public Analyzer getAnalyzer() {
		if (analyzer == null && !StringUtils.isEmpty(analyzerClass))
			initAnalyzer();

		if (analyzer == null) {
			analyzer = new SimpleAnalyzer();
			log.debug("Using default simple analyzer");
		}

		analyzer.setVersion(StandardSearchEngine.VERSION);

		return analyzer;
	}

	private void initAnalyzer() {
		// Try to instantiate the specified analyzer (Using default
		// constructor)
		Class<?> aClass = null;
		try {
			aClass = Class.forName(analyzerClass);
		} catch (Exception t) {
			log.error("{} not found", analyzerClass);
		}

		// Try to use constructor (Set<?> stopwords)
		if (aClass != null && stopWords != null && (!stopWords.isEmpty())) {
			try {
				Constructor<?> constructor = aClass.getConstructor(java.util.Set.class);
				if (constructor != null)
					analyzer = (Analyzer) constructor.newInstance(stopWords);
			} catch (Exception e) {
				log.debug("constructor (Version matchVersion, Set<?> stopwords)  not found");
			}
		}

		// Try with default constructor
		if (aClass != null && analyzer == null) {
			try {
				analyzer = (Analyzer) aClass.getDeclaredConstructor().newInstance();
			} catch (Exception e) {
				log.debug("constructor without arguments not found");
			}
		}
	}

	public void setAnalyzer(Analyzer analyzer) {
		this.analyzer = analyzer;
	}

	@Override
	public String toString() {
		return locale.toString();
	}

	@Override
	public int hashCode() {
		return toString().hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (getClass() != obj.getClass())
			return false;

		return toString().equals(obj.toString());
	}

	@Override
	public int compareTo(Language o) {
		if ("standard".equals(toString()))
			return -1;
		else if ("standard".equals(o.toString()))
			return 1;
		else
			return toString().compareToIgnoreCase(o.toString());
	}

	public SnowballProgram getStemmer() {
		if (stemmer == null) {
			String stemmerClass = "org.tartarus.snowball.ext." + getLocale().getDisplayName(Locale.ENGLISH) + "Stemmer";

			try {
				Class<?> clazz = Class.forName(stemmerClass);
				if (clazz != null) {
					Constructor<?> constructor = clazz.getConstructor();
					if (constructor != null)
						stemmer = (SnowballProgram) constructor.newInstance();
				}
			} catch (Exception t) {
				// Nothing to do
			}
		}
		return stemmer;
	}
}