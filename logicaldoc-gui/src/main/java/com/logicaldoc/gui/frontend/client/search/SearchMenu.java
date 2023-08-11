package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

/**
 * The left menu in the search area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchMenu extends SectionStack {

	private static final String FULLTEXT = "fulltext";

	private static final String FOLDERS = "folders";

	private static final String TAGS_SECTION = "tags";

	private static final String PARAMETRIC_SECTION = "parametric";

	private static SearchMenu instance;

	public static SearchMenu get() {
		if (instance == null)
			instance = new SearchMenu();
		return instance;
	}

	private SearchMenu() {
		setVisibilityMode(VisibilityMode.MUTEX);

		for (String searchType : Session.get().getUser().orderedSearches()) {
			addSearchSection(searchType);
		}

		addSavedSearchesSection();
	}

	private void addSearchSection(String searchType) {
		if (FULLTEXT.equals(searchType)) {
			SectionStackSection fulltextSection = new SectionStackSection(I18N.message(FULLTEXT));
			fulltextSection.setName(FULLTEXT);
			fulltextSection.setItems(new FulltextForm());
			addSection(fulltextSection);
		} else if ("tags".equals(searchType) && Feature.visible(Feature.TAGS)) {
			SectionStackSection tagsSection = new SectionStackSection(I18N.message("tags"));
			tagsSection.setName(TAGS_SECTION);
			if (Feature.enabled(Feature.TAGS))
				tagsSection.setItems(TagsForm.get());
			else
				tagsSection.setItems(new FeatureDisabled());
			addSection(tagsSection);
		} else if ("parameters".equals(searchType) && Feature.visible(Feature.PARAMETRIC_SEARCHES)) {
			SectionStackSection parametricSection = new SectionStackSection(I18N.message("parameters"));
			parametricSection.setName(PARAMETRIC_SECTION);
			if (Feature.enabled(Feature.PARAMETRIC_SEARCHES))
				parametricSection.setItems(ParametricForm.get());
			else
				parametricSection.setItems(new FeatureDisabled());
			addSection(parametricSection);
		} else if (FOLDERS.equals(searchType)) {
			SectionStackSection foldersSection = new SectionStackSection(I18N.message(FOLDERS));
			foldersSection.setName(FOLDERS);
			foldersSection.setExpanded(false);
			foldersSection.setItems(FoldersForm.get());
			addSection(foldersSection);
		}
	}

	private void addSavedSearchesSection() {
		if (Feature.visible(Feature.SAVED_SEARCHES)) {
			SectionStackSection savedSection = new SectionStackSection(I18N.message("savedsearches"));
			savedSection.setName("saved");
			savedSection.setExpanded(false);
			if (Feature.enabled(Feature.SAVED_SEARCHES))
				savedSection.setItems(SavedSearchesPanel.get());
			else
				savedSection.setItems(new FeatureDisabled());
			addSection(savedSection);
		}
	}

	public void openFulltextSection() {
		expandSection(FULLTEXT);
	}

	public void openTagsSection() {
		expandSection(TAGS_SECTION);
	}

	public void openFoldersSection() {
		expandSection(FOLDERS);
	}

	public void openParametricSection() {
		if (Feature.visible(Feature.PARAMETRIC_SEARCHES))
			expandSection(PARAMETRIC_SECTION);
	}
}