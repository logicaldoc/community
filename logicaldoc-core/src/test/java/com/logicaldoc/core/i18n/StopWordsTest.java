package com.logicaldoc.core.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Locale;
import java.util.Set;

import org.junit.Test;

/**
 * Test case for {@link StopWords}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class StopWordsTest {
	
	@Test
	public void testGetStopWords() {
		// 'en' locale
		Set<String> words = StopWords.getStopWords(Locale.ENGLISH);
		assertNotNull(words);
		assertEquals(true, words.contains("a"));

		// 'de' locale
		words = StopWords.getStopWords(Locale.GERMAN);
		assertNotNull(words);
		assertEquals(true, words.contains("wessen"));

		// 'fr' locale
		words = StopWords.getStopWords(Locale.FRANCE);
		assertNotNull(words);
		assertEquals(true, words.contains("alors"));

		// 'it' locale
		words = StopWords.getStopWords(Locale.ITALIAN);
		assertNotNull(words);
		assertEquals(true, words.contains("dei"));
		
		words = StopWords.getStopWords("en");
		assertNotNull(words);
		assertEquals(true, words.contains("a"));
	}
}
