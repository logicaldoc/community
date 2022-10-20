package com.logicaldoc.gui.frontend.client.docusign;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.note.AbstractAnnotationsWindow;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to define the placehoders of a document where the
 * recipients have to sign
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class DocuSignTabsEditor extends AbstractAnnotationsWindow {

	public DocuSignTabsEditor(GUIDocument doc) {
		super(doc, null, Arrays.asList("esig-signhere", "esig-text", "esig-date", "esig-company", "esig-datesigned",
				"esig-title", "esig-fullname", "esig-envelopeid", "esig-email", "esig-emailaddress"));
		setTitle(I18N.message("docusigntabs") + " - " + doc.getFileName());
	}

	@Override
	protected void onNotesSaved() {
		destroy();
	}

	@Override
	protected DrawItem prepareAnnotationItem(GUIDocumentNote note) {
		DrawItem drawItem = newAnnotationItem(note);
		if (Session.get().getUser().isMemberOf("admin") || note.getUserId() == Session.get().getUser().getId()) {
			DocuSignTabContextMenu contextMenu = new DocuSignTabContextMenu(drawItem, note);
			contextMenu.addDeleteClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {

				@Override
				public void onClick(com.smartgwt.client.widgets.menu.events.MenuItemClickEvent event) {
					LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
						@Override
						public void execute(Boolean value) {
							if (value) {
								notes.remove(note);
								drawItem.erase();
							}
						}
					});
				}
			});
			drawItem.setContextMenu(contextMenu);
		}
		return drawItem;
	}

	@Override
	protected void prepareAdditionalActions(ToolStrip toolStrip) {
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addtab"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				DocuSignNewTabDialog dialog = new DocuSignNewTabDialog(new AsyncCallback<GUIDocumentNote>() {

					@Override
					public void onSuccess(GUIDocumentNote newNote) {
						newNote.setUsername(Session.get().getUser().getFullName());
						newNote.setMessage(I18N.message("annotation.type." + newNote.getType()));
						newNote.setDocId(document.getId());
						newNote.setFileVersion(fileVersion);
						newNote.setPage((Integer) pageCursor.getValue());
						newNote.setShowKnobs(true);
						notes.add(newNote);

						showCurrentPage();
					}

					@Override
					public void onFailure(Throwable arg0) {
						// Nothing to do
					}
				});
				dialog.show();
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
	}
}