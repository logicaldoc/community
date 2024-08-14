package com.logicaldoc.core.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Locale;
import java.util.Set;

import org.junit.Test;
import org.tartarus.snowball.SnowballProgram;

public class LanguageTest {

	@Test
	public void testGetStopWords() {
		Language language = new Language(Locale.FRENCH);
		Set<String> swords = language.getStopWords();
		assertNotNull(swords);
		assertEquals(126, swords.size());
	}

	@Test
	public void testGetLocale() {
		Language language = new Language(Locale.GERMAN);
		assertEquals(Locale.GERMAN, language.getLocale());
	}

	@Test
	public void testGetStemmer() {
		Language language = new Language(Locale.GERMAN);
		SnowballProgram sbp = language.getStemmer();
		assertNotNull(sbp);
		assertEquals("org.tartarus.snowball.ext.GermanStemmer", sbp.getClass().getName());

		// Test for Arabic
		Locale locale = Locale.forLanguageTag("ar");

		language = new Language(locale);
		sbp = language.getStemmer();
		assertNotNull(sbp);
		assertEquals("org.tartarus.snowball.ext.ArabicStemmer", sbp.getClass().getName());

		// Polish
		locale = Locale.forLanguageTag("pl");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp); // NO stemmer for Polish

		// Bulgarian
		locale = Locale.forLanguageTag("bg");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp); // NO stemmer for Bulgarian

		// Albanian
		locale = Locale.forLanguageTag("sq");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp); // NO stemmer for Albanian
	}

	@Test
	public void testSetAnalyzerClass() {
		Language lang = new Language(Locale.GERMAN);
		String analyzer = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		lang.setAnalyzerClass(analyzer);
		assertNotNull(lang.getAnalyzer());
		assertEquals(analyzer, lang.getAnalyzerClass());

		// Arabic
		Locale locale = Locale.forLanguageTag("ar");
		lang = new Language(locale);

		assertNotNull(lang.getAnalyzer());
		assertNull(lang.getAnalyzerClass());
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());

		// Bulgarian
		locale = Locale.forLanguageTag("bg");
		lang = new Language(locale);
		assertNotNull(lang.getAnalyzer());
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());

		// Albanian
		locale = Locale.forLanguageTag("sq");
		lang = new Language(locale);
		assertNotNull(lang.getAnalyzer());
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());
	}
}