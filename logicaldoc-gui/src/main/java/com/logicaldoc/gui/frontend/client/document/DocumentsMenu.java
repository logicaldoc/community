package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.folder.FolderCursor;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigatorPanel;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.events.SectionHeaderClickEvent;
import com.smartgwt.client.widgets.layout.events.SectionHeaderClickHandler;

/**
 * The left menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsMenu extends SectionStack {

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
		} catch (Throwable t) {
			setWidth(280);
		}

		setShowResizeBar(true);

		browser = new SectionStackSection(I18N.message("browser") + "   ");
		browser.setName("browser");
		browser.setCanCollapse(true);
		if (Session.get().isFolderPagination()) {
			browser.setControls(FolderCursor.get());
		}

		browser.setItems(FolderNavigatorPanel.get());
		if (showBrowser)
			addSection(browser);

		if (showBookmarks && Feature.visible(Feature.BOOKMARKS)) {
			bookmarksSection = new SectionStackSection(I18N.message("bookmarks"));
			bookmarksSection.setName("bookmarks");
			bookmarksSection.setCanCollapse(true);

			if (Feature.enabled(Feature.BOOKMARKS))
				bookmarksSection.setItems(BookmarksPanel.get());
			else
				bookmarksSection.addItem(new FeatureDisabled());
			addSection(bookmarksSection);
		}

		if (showTrash && Feature.visible(Feature.TRASH)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TRASH)) {
			trashSection = new SectionStackSection(I18N.message("trash"));
			trashSection.setName("trash");
			trashSection.setCanCollapse(true);
			if (Feature.enabled(Feature.TRASH))
				trashSection.setItems(TrashPanel.get());
			else
				trashSection.addItem(new FeatureDisabled());
			addSection(trashSection);
		}

		addSectionHeaderClickHandler(new SectionHeaderClickHandler() {
			@Override
			public void onSectionHeaderClick(SectionHeaderClickEvent event) {
				if (event.getSection() != null) {
					refresh(event.getSection().getAttributeAsString("name"));
				}
			}
		});

		addResizedHandler(new ResizedHandler() {
			@Override
			public void onResized(ResizedEvent event) {
				if (initialized) {
					// Save the new width in a cookie
					CookiesManager.save(CookiesManager.COOKIE_DOCSMENU_W, getWidthAsString());
				} else
					initialized = true;
			}
		});
	}

	public void refresh(String sectionNameToExpand) {
		if ("bookmarks".equals(sectionNameToExpand)) {
			BookmarksPanel.get().refresh();
		} else if ("trash".equals(sectionNameToExpand)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TRASH)) {
			TrashPanel.get().refresh();
		}
	}
}