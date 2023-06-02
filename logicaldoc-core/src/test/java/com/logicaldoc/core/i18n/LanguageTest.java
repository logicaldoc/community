package com.logicaldoc.core.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.util.Locale;
import java.util.Set;

import org.junit.Test;
import org.tartarus.snowball.SnowballProgram;

public class LanguageTest {

	@Test
	public void testGetStopWords() throws IOException {
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
		//System.out.println(sbp.getClass().getName());	
		assertEquals("org.tartarus.snowball.ext.ArabicStemmer", sbp.getClass().getName());
		
		// Polish
		locale = Locale.forLanguageTag("pl");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp);  // NO stemmer for Polish
		
		// Bulgarian
		locale = Locale.forLanguageTag("bg");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp);  // NO stemmer for Bulgarian
		
		// Albanian
		locale = Locale.forLanguageTag("sq");
		language = new Language(locale);
		sbp = language.getStemmer();
		assertNull(sbp);  // NO stemmer for Albanian		
	}	
	
	
	@Test
	public void testSetAnalyzerClass() {
		Language lang = new Language(Locale.GERMAN);
		String analyzer = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		System.out.println(analyzer);
		
		lang.setAnalyzerClass(analyzer);
		
		System.out.println(lang.getAnalyzer());
		System.out.println(lang.getAnalyzerClass());
		
		assertNotNull(lang.getAnalyzer());
		assertEquals(analyzer, lang.getAnalyzerClass());
		
		// Arabic
		Locale locale = Locale.forLanguageTag("ar");
		lang = new Language(locale);
		analyzer = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		System.out.println(analyzer);
		
		assertNotNull(lang.getAnalyzer());
		System.out.println(lang.getAnalyzer());
		System.out.println(lang.getAnalyzerClass());
		assertNull(lang.getAnalyzerClass());
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());
		
		// Bulgarian
		locale = Locale.forLanguageTag("bg");
		lang = new Language(locale);
		analyzer = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		System.out.println(analyzer);
		
		assertNotNull(lang.getAnalyzer());
		System.out.println(lang.getAnalyzer());
		System.out.println(lang.getAnalyzerClass());		
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());
		
		// Albanian
		locale = Locale.forLanguageTag("sq");
		lang = new Language(locale);
		analyzer = "org.apache.lucene.analysis." + lang.getLanguage() + "."
				+ lang.getLocale().getDisplayName(Locale.ENGLISH) + "Analyzer";
		System.out.println(analyzer);
		
		assertNotNull(lang.getAnalyzer());
		System.out.println(lang.getAnalyzer());
		System.out.println(lang.getAnalyzerClass());		
		assertEquals("org.apache.lucene.analysis.core.SimpleAnalyzer", lang.getAnalyzer().getClass().getName());
		
	}	
	
	//
	// private void convertFiles() throws IOException {
	// File sss = new
	// File("C:/Users/alle/workspace46/logicaldoc/logicaldoc-core/src/main/resources/stopwords");
	//
	// FileFilter myff = new FileFilter(){
	//
	// public boolean accept(File arg0) {
	// if (arg0.getName().endsWith(".txt"))
	// return true;
	// return false;
	// }};
	//
	// File[] fl = sss.listFiles(myff);
	// for (int i = 0; i < fl.length; i++) {
	// FileReader fr = new FileReader(fl[i]);
	// BufferedReader br = new BufferedReader(fr);
	//
	// File newFile = new File(fl[i].getParent(), fl[i].getName() + ".new");
	//
	// FileOutputStream fos = new FileOutputStream(newFile);
	// OutputStreamWriter osw = new OutputStreamWriter(fos, "UTF-8");
	// //FileWriter fw = new FileWriter(newFile);
	// BufferedWriter bw = new BufferedWriter(osw);
	//
	// String line = null;
	// while((line = br.readLine()) != null) {
	// bw.write(line + "\r\n");
	// }
	//
	// bw.flush();
	//
	// bw.close();
	// osw.close();
	// fos.close();
	// br.close();
	// fr.close();
	// }
	// }
}