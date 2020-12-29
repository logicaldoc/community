package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.RightMouseDownEvent;
import com.smartgwt.client.widgets.events.RightMouseDownHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * A canvas that contains a note to be displayed in a page of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class AnnotationCanvas extends Label {

	protected GUIDocumentNote note;

	protected AbstractAnnotationsWindow annotationsWindow;
	
	private boolean writeEnabled=true;

	public AnnotationCanvas(GUIDocumentNote note, final AbstractAnnotationsWindow annotationsWindow, boolean writeAllowed) {
		this.note = note;
		this.annotationsWindow = annotationsWindow;
		writeEnabled = writeAllowed && (Session.get().getUser().isMemberOf("admin")
				|| note.getUserId() == Session.get().getUser().getId());

		setOverflow(Overflow.HIDDEN);
		setBorder("1px solid #AAAAAA");

		setCanDragReposition(writeEnabled);
		setDragAppearance(writeEnabled ? DragAppearance.TARGET : DragAppearance.NONE);
		setCanDragResize(writeEnabled);
		setSmoothFade(true);

		addDoubleClickHandler(new DoubleClickHandler() {

			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		if (writeEnabled) {
			addRightMouseDownHandler(new RightMouseDownHandler() {

				@Override
				public void onRightMouseDown(RightMouseDownEvent event) {
					showContextMenu();
					event.cancel();
				}
			});
		}
	}

	@Override
	public void onDraw() {
		setBackgroundColor(note.getColor());
		setOpacity(note.getOpacity());
		setContents(note.getMessage());

		ImageWithCanvases pageImage = annotationsWindow.getPageImage();
		setLeft(pageImage.computeLeftPixel(note.getLeft()));
		setTop(pageImage.computeTopPixel(note.getTop()));
		setWidth(pageImage.computeWidthPixels(note.getWidth()));
		setHeight(pageImage.computeHeightPixels(note.getHeight()));
	}

	protected void onEdit() {
		AnnotationEditor editor = new AnnotationEditor(note, writeEnabled) {
			@Override
			void onSave() {
				super.onSave();
				AnnotationCanvas.this.setContents(note.getMessage());
				AnnotationCanvas.this.setBackgroundColor(note.getColor());
				AnnotationCanvas.this.setOpacity(note.getOpacity());
			}
		};
		editor.show();
	}

	public void captureNotePosition() {
		ImageWithCanvases pageImage = annotationsWindow.getPageImage();
		note.setLeft(pageImage.getLeft(getLeft()));
		note.setTop(pageImage.getTop(getTop()));
		note.setWidth(pageImage.getWidth(getWidth()));
		note.setHeight(pageImage.getHeight(getHeight()));
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				annotationsWindow.deleteNote(note);
			}
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onEdit();
			}
		});

		contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}
}