package com.logicaldoc.core.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.apache.lucene.analysis.Analyzer;
import org.junit.Test;
import org.tartarus.snowball.SnowballProgram;

/**
 * Test case for {@link Language}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
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
		Language language1 = new Language(Locale.GERMAN);
		assertEquals(Locale.GERMAN, language1.getLocale());

		Language language2 = new Language(Locale.ITALIAN);
		assertEquals(Locale.ITALIAN, language2.getLocale());

		assertNotSame(language1.hashCode(), language2.hashCode());
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
		lang.setAnalyzer(null);

		lang = new Language(Locale.ITALIAN);
		String analyzerClass = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		lang.setAnalyzerClass(analyzerClass);

		Set<String> customStopWords = new HashSet<>();
		customStopWords.add("lì");
		customStopWords.add("lui");
		customStopWords.add("tua");
		customStopWords.add("avevamo");
		customStopWords.add("stessero");
		lang.setStopWords(customStopWords);

		Analyzer analyzer1 = lang.getAnalyzer();
		assertNotNull(analyzer1);
		assertNotSame(lang.getStopWords().isEmpty(), lang.getStopWords());
	}

	@SuppressWarnings("unlikely-arg-type")
	@Test
	public void testCompareToAndHashCode() {
		Language language1 = new Language(Locale.GERMAN);
		assertEquals(Locale.GERMAN, language1.getLocale());

		Language language2 = new Language(Locale.ITALIAN);
		assertEquals(Locale.ITALIAN, language2.getLocale());

		assertNotNull(language2.toString());

		String nullObj = null;
		assertEquals(false, language1.equals(nullObj));
		String stringObj = "notALanguage";
		assertEquals(false, language1.equals(stringObj));

		assertEquals(true, language1.compareTo(language2) < 0);
		assertEquals(0, language1.compareTo(language1));
		assertEquals(true, language1.equals(language1));

		Language language3 = new Language(Locale.KOREAN);
		assertEquals("Korean", language3.getDisplayLanguage());
		assertNotSame("English", language3.getDefaultDisplayLanguage());

		Locale locale = Locale.of("standard");
		language3 = new Language(locale);
		assertEquals(1, language1.compareTo(language3));
		assertEquals(-1, language3.compareTo(language3));
	}
}