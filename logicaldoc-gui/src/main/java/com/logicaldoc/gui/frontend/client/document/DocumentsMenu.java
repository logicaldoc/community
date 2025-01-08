package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigatorPanel;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderCursor;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;

/**
 * The left menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsMenu extends SectionStack {

	private static final String BOOKMARKS = "bookmarks";

	private static final String TRASH = "trash";

	protected SectionStackSection browser = null;

	protected SectionStackSection bookmarksSection = null;

	protected SectionStackSection trashSection = null;

	private boolean initialized = false;

	public DocumentsMenu() {
		this(true, true, true);
	}

	public DocumentsMenu(boolean showBrowser, boolean showBookmarks, boolean showTrash) {
		setVisibilityMode(VisibilityMode.MUTEX);

		try {
			// Retrieve the saved menu width
			String w = CookiesManager.get(CookiesManager.COOKIE_DOCSMENU_W);
			setWidth(Integer.parseInt(w));
		} catch (Exception t) {
			setWidth(280);
		}

		setShowResizeBar(true);

		addBrowser(showBrowser);

		addBookmarks(showBookmarks);

		addTrash(showTrash);

		addSectionHeaderClickHandler(event -> {
			if (event.getSection() != null) {
				refresh(event.getSection().getAttributeAsString("name"));
			}
		});

		addResizedHandler(event -> {
			if (initialized) {
				// Save the new width in a cookie
				CookiesManager.save(CookiesManager.COOKIE_DOCSMENU_W, getWidthAsString());
			} else
				initialized = true;
		});
	}

	private void addTrash(boolean showTrash) {
		if (showTrash && Feature.visible(Feature.TRASH)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TRASH)) {
			trashSection = new SectionStackSection(I18N.message(TRASH));
			trashSection.setName(TRASH);
			trashSection.setCanCollapse(true);
			if (Feature.enabled(Feature.TRASH))
				trashSection.setItems(TrashPanel.get());
			else
				trashSection.addItem(new FeatureDisabled());
			addSection(trashSection);
		}
	}

	private void addBookmarks(boolean showBookmarks) {
		if (showBookmarks && Feature.visible(Feature.BOOKMARKS)) {
			bookmarksSection = new SectionStackSection(I18N.message(BOOKMARKS));
			bookmarksSection.setName(BOOKMARKS);
			bookmarksSection.setCanCollapse(true);

			if (Feature.enabled(Feature.BOOKMARKS))
				bookmarksSection.setItems(BookmarksPanel.get());
			else
				bookmarksSection.addItem(new FeatureDisabled());
			addSection(bookmarksSection);
		}
	}

	private void addBrowser(boolean showBrowser) {
		browser = new SectionStackSection(I18N.message("browser") + "   ");
		browser.setName("browser");
		browser.setCanCollapse(true);
		if (Session.get().isFolderPagination()) {
			browser.setControls(FolderCursor.get());
		}

		browser.setItems(FolderNavigatorPanel.get());
		if (showBrowser)
			addSection(browser);
	}

	public void refresh(String sectionNameToExpand) {
		if (BOOKMARKS.equals(sectionNameToExpand)) {
			BookmarksPanel.get().refresh();
		} else if (TRASH.equals(sectionNameToExpand)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TRASH)) {
			TrashPanel.get().refresh();
		}
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}