package com.logicaldoc.gui.frontend.client.annnotation;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.document.NotesPanel;
import com.logicaldoc.gui.frontend.client.services.AnnotationsService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class AnnotationEditor extends Window {

	private NotesPanel notesPanel;

	private AnnotationsWindow annotationsWindow;

	private ButtonItem save;

	private Long noteId;

	private RichTextItem message;

	private long docId;

	private int page;

	private String snippet;

	private DynamicForm noteForm = new DynamicForm();

	public AnnotationEditor(final long docId, final Long noteId, final NotesPanel notesPanel,
			final AnnotationsWindow annotationsWindow, String text, String snippet, final int page) {
		super();
		this.notesPanel = notesPanel;
		this.annotationsWindow = annotationsWindow;
		this.noteId = noteId;
		this.docId = docId;
		this.page = page;
		this.snippet = snippet;

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				try {
					removeTempAnnotationFromHTML();
				} catch (Throwable t) {
				}
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (notesPanel != null)
			setTitle(I18N.message("note"));
		else
			setTitle(I18N.message("annotation"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		message = new RichTextItem("name");
		message.setTitle(I18N.message("message"));
		message.setShowTitle(false);
		message.setRequired(true);
		message.setWidth(680);
		message.setHeight(230);
		message.setValue(text);

		save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		noteForm.setItems(message, save);
		addItem(noteForm);
	}

	private void onSave() {
		if (!noteForm.validate())
			return;
		if (notesPanel == null) {
			if (noteId == null) {
				AnnotationsService.Instance.get().addAnnotation(docId, page, snippet, message.getValue().toString(),
						new AsyncCallback<Long>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);

								try {
									removeTempAnnotationFromHTML();
								} catch (Throwable t) {
								}

								destroy();
							}

							@Override
							public void onSuccess(Long noteId) {
								try {
									updateTempAnnotationToHTML("" + noteId);
								} catch (Throwable t) {
								}

								AnnotationsService.Instance.get().savePage(docId, page, getPageContent(),
										new AsyncCallback<Void>() {

											@Override
											public void onFailure(Throwable caught) {
												Log.serverError(caught);

												try {
													removeTempAnnotationFromHTML();
												} catch (Throwable t) {
												}

												destroy();
											}

											@Override
											public void onSuccess(Void arg) {
												annotationsWindow.showPage(page);
												destroy();
											}
										});
							}
						});
			} else {
				DocumentService.Instance.get().updateNote(docId, noteId, message.getValue().toString(),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
								destroy();
							}

							@Override
							public void onSuccess(Void result) {
								annotationsWindow.onUpdated(message.getValue().toString());
								destroy();
							}
						});
			}
		} else {
			if (noteId == null) {
				DocumentService.Instance.get().addNote(docId, message.getValue().toString(), new AsyncCallback<Long>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(Long result) {
						AnnotationEditor.this.notesPanel.onAdded(result.longValue(), message.getValue().toString());
						destroy();
					}
				});
			} else {
				DocumentService.Instance.get().updateNote(docId, noteId, message.getValue().toString(),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
								destroy();
							}

							@Override
							public void onSuccess(Void result) {
								AnnotationEditor.this.notesPanel.onUpdated(message.getValue().toString());
								destroy();
							}
						});
			}
		}
	}

	/**
	 * Updates the annotation to the current selection
	 * 
	 * @param annotationId
	 */
	public native void updateTempAnnotationToHTML(String annotationId)/*-{
		$wnd.annUpdateAnnotation("newannotationid", annotationId);
	}-*/;

	/**
	 * Removes the temporary annotation from the content
	 */
	public native void removeTempAnnotationFromHTML()/*-{
		$wnd.annRemoveAnnotation("newannotationid");
	}-*/;

	/**
	 * Gets the full HTML of the current page.
	 */
	public native String getPageContent()/*-{
		var markup = $wnd.annGetContent().document.documentElement.innerHTML;
		return markup;
	}-*/;
}