package com.logicaldoc.core.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.Locale;
import java.util.Set;

import org.junit.Test;

public class LanguageTest {

	@Test
	public void testLoadStopwords() throws IOException {
		Language language = new Language(Locale.FRENCH);
		Set<String> swords = language.getStopWords();
		assertNotNull(swords);
		assertEquals(126, swords.size());
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