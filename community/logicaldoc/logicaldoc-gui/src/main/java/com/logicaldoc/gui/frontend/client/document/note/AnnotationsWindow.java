package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.google.gwt.dom.client.ImageElement;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.widgetideas.graphics.client.ImageLoader.CallBack;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to add a note in a visually defined position
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class AnnotationsWindow extends Window {

	private SpinnerItem pageCursor;

	private HLayout bottom = new HLayout();

	private GUIDocument document;

	private ImageWithCanvases pageImage;

	private NotesPanel notesPanel;

	private List<GUIDocumentNote> notes = new ArrayList<GUIDocumentNote>();

	private String fileVersion = null;

	private boolean writeEnabled = true;

	public AnnotationsWindow(GUIDocument doc, String fileVer, NotesPanel notesPanel, boolean writeEnabled) {
		this.document = doc;
		this.notesPanel = notesPanel;
		this.writeEnabled = writeEnabled;
		this.fileVersion = fileVer;
		if (this.fileVersion == null)
			this.fileVersion = doc.getFileVersion();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("annotations") + " - " + doc.getFileName() + " v" + fileVersion);
		setWidth100();
		setHeight100();

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		DocumentService.Instance.get().getNotes(document.getId(), fileVersion, new AsyncCallback<GUIDocumentNote[]>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocumentNote[] nts) {
				notes.addAll(Arrays.asList(nts));
				initGUI();
			}
		});
	}

	private void showPage(int page) {
		if (pageImage != null) {
			captureNotesPosition();
			bottom.removeMembers(bottom.getMembers());
		}

		pageImage = new ImageWithCanvases(getPageUrl(page), null, new CallBack() {

			@Override
			public void onImagesLoaded(ImageElement[] imageElements) {
				// Reload the document to update the pages count
				if (document.getPages() <= 1)
					DocumentService.Instance.get().getById(document.getId(), new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument dc) {
							document.setPages(dc.getPages());
							pageCursor.setMax(document.getPages());
							pageCursor.setHint("/" + (document.getPages() > 0 ? document.getPages() : 1));
						}
					});
				showAnnotations(page);
			}
		});
		bottom.addMember(pageImage);
	}

	private String getPageUrl(int page) {
		return Util.contextPath() + "convertjpg?docId=" + document.getId() + "&fileVersion=" + fileVersion + "&page="
				+ page + "&random=" + new Date().getTime();
	}

	private void showAnnotations(int page) {
		for (GUIDocumentNote note : notes)
			if (note.getPage() == page) {
				Canvas noteCanvas = new AnnotationCanvas(note, this, writeEnabled);
				pageImage.addCanvas(noteCanvas);
			}
	}

	private void captureNotesPosition() {
		if (pageImage != null && pageImage.getCanvases() != null) {
			List<Canvas> canvases = pageImage.getCanvases();
			for (Canvas canvas : canvases) {
				AnnotationCanvas noteCanvas = (AnnotationCanvas) canvas;
				noteCanvas.captureNotePosition();
			}
		}
	}

	private void onSave() {
		captureNotesPosition();
		DocumentService.Instance.get().saveNotes(document.getId(), notes.toArray(new GUIDocumentNote[0]),
				new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						if (notesPanel != null)
							notesPanel.refresh();
						destroy();
					}
				});
	}

	public void deleteNote(GUIDocumentNote note) {
		notes.remove(note);
		showCurrentPage();
	}

	private void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		ToolStripButton zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (pageImage != null) {
					captureNotesPosition();
					pageImage.clearCanvases();
					pageImage.resize(100);
					showAnnotations((Integer) pageCursor.getValue());
				}
			}
		});

		ToolStripButton zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (pageImage != null) {
					captureNotesPosition();
					pageImage.clearCanvases();
					pageImage.resize(-100);
					showAnnotations((Integer) pageCursor.getValue());
				}
			}
		});

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addannotation"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				captureNotesPosition();

				GUIDocumentNote note = new GUIDocumentNote();
				note.setDocId(document.getId());
				note.setUserId(Session.get().getUser().getId());
				note.setUsername(Session.get().getUser().getFullName());
				note.setMessage(I18N.message("annotation").toLowerCase() + "...");
				note.setPage((Integer) pageCursor.getValue());
				notes.add(note);

				Canvas noteCanvas = new AnnotationCanvas(note, AnnotationsWindow.this, writeEnabled);
				pageImage.addCanvas(noteCanvas);
			}
		});

		pageCursor = ItemFactory.newSpinnerItem("page", "page", 1, 1,
				document.getPages() > 0 ? document.getPages() : 1);
		pageCursor.setHint("/" + (document.getPages() > 0 ? document.getPages() : 1));
		pageCursor.setSaveOnEnter(true);
		pageCursor.setImplicitSave(true);
		pageCursor.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				showCurrentPage();
			}
		});

		toolStrip.addFormItem(pageCursor);
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addSeparator();

		if (writeEnabled) {
			toolStrip.addButton(add);
			toolStrip.addSeparator();
			toolStrip.addButton(save);
		}
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(bottom);

		showPage(1);
	}

	private void showCurrentPage() {
		showPage((Integer) pageCursor.getValue());
	}

	public ImageWithCanvases getPageImage() {
		return pageImage;
	}
}