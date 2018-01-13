package com.logicaldoc.core.searchengine.analyzer;

import java.util.HashMap;
import java.util.Locale;

import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.snowball.SnowballFilter;
import org.apache.lucene.analysis.util.TokenFilterFactory;
import org.tartarus.snowball.SnowballProgram;

import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.util.LocaleUtil;

/**
 * Retrieves a token stream elaborated by Snowaball Filter
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class SnowballFilterFactory extends TokenFilterFactory {

	private Language language;

	public SnowballFilterFactory(HashMap<String, String> config) {
		super(config);

		String lang = config.get("lang");
		LanguageManager man = LanguageManager.getInstance();
		language = man.getLanguage(LocaleUtil.toLocale(lang));
		if (language == null)
			language = new Language(Locale.ENGLISH);
	}

	@Override
	public TokenStream create(TokenStream ts) {
		// Try to add a snowball filter
		SnowballProgram stemmer = language.getStemmer();

		if (stemmer != null)
			return new SnowballFilter(ts, stemmer);
		else
			return ts;
	}

}
