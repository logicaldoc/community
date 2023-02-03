package com.logicaldoc.i18n;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.StringTokenizer;

public class PurifyRB {

	public static void main(String[] args) throws IOException {

		List<Locale> locales = new ArrayList<Locale>();
		Properties loc = new Properties();
		loc.load(PurifyRB.class.getResourceAsStream("/i18n/i18n.properties"));
		StringTokenizer st = new StringTokenizer(loc.getProperty("locales"), ",", false);
		while (st.hasMoreElements()) {
			String elem = (String) st.nextElement();
			locales.add(toLocale(elem));
		}

		List<String> bundles = new ArrayList<>();
		st = new StringTokenizer(loc.getProperty("bundles"), ",", false);
		while (st.hasMoreElements()) {
			String elem = (String) st.nextElement();
			bundles.add(elem);
		}

		for (String bundle : bundles) {
			System.out.println("\nInspecting bundle " + bundle);

			ResourceBundle master = ResourceBundle.getBundle("i18n/" + bundle, Locale.ENGLISH);
			for (Locale locale : locales) {
				try {
					System.out.println("\nAnalyzing language: " + locale);
					writePurifiedRB(master, "i18n/" + bundle, locale);
				} catch (Exception e) {
					System.out.println("Locale: " + locale + ", NOT FOUND");
					e.printStackTrace();
				}
			}
		}

		System.out.println("Finished");
	}

	private static void writePurifiedRB(ResourceBundle master, String transalatedBundle, Locale locale) {
		ResourceBundle translated = ResourceBundle.getBundle(transalatedBundle, locale);
		if (locale != translated.getLocale()) {
			System.out.println("Translation for locale: " + locale + ", not Found; Skipped");
			return;
		}

		Properties destprop = new OrderedProperties();

		// 1: controllare che nel bundle dest i valori delle chiavi siano
		// diversi da quelli nel bundle source

		int counter = 0;
		int mre = 0;

		Set<String> keySet = translated.keySet();
		for (String key : keySet) {
			String value = translated.getString(key);
			try {
				String masterValue = master.getString(key);

				if (masterValue != null) {
					// la chiave esiste anche nel master
					if (!value.equals(masterValue)) {
						// il valore e diverso da quello iniziale
						// copio l'accoppiata chiave valore nel properties di
						// destinazione
						destprop.put(key, value);
						// System.out.println("trans <> master");
						counter++;
					}
				}
			} catch (MissingResourceException e) {
				mre++;
			}
		}

		System.out.println("trans props n.: " + translated.keySet().size());
		System.out.println("written props n.: " + counter);
		System.out.println("exceding keywords n.: " + mre);

		System.out.println(locale);
		File dir = new File("target/po/i18n");
		dir.mkdir();
		dir.mkdirs();

		File file = new File("target/po/" + transalatedBundle + "_" + locale + ".properties");

		try (FileOutputStream out = new FileOutputStream(file);
				OutputStreamWriter ow = new OutputStreamWriter(out, "UTF-8");) {
			// scrivo il prop destinazione su FileSystem
			destprop.store(ow, "comments");
			ow.flush();
			out.flush();
			// out.close();
			// ow.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static Locale toLocale(String str) {
		String lang = "";
		String country = "";
		String variant = "";
		StringTokenizer st = new StringTokenizer(str, "_", false);
		lang = st.nextToken();
		if (st.hasMoreTokens())
			country = st.nextToken();
		if (st.hasMoreTokens())
			variant = st.nextToken();
		return new Locale(lang, country, variant);
	}
}
