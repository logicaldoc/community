package com.logicaldoc.gui.frontend.client.annnotation;

import java.util.Date;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.AnnotationsService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is a mini-app to handle annotations
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.2
 */
public class AnnotationsWindow extends Window {

	private HTMLPane contentPane;

	private VLayout pageContent = new VLayout();

	private long docId;

	private int page = 1;

	private ListGrid notesGrid;

	private HLayout frame = new HLayout();

	private VLayout annotations = new VLayout();

	private boolean handlingOnOver = false;

	public AnnotationsWindow(final long docId, String title, int pages) {
		super();

		this.docId = docId;

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("annotations") + " - " + title);
		setWidth100();
		setHeight100();
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton addAnnotation = new ToolStripButton();
		addAnnotation.setTitle(I18N.message("addannotation"));
		addAnnotation.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				addTempAnnotationToHTML();
			}
		});

		SpinnerItem pageCursor = ItemFactory.newSpinnerItem("page", "page", 1, 1, pages);
		pageCursor.setHint("/" + pages);
		pageCursor.setSaveOnEnter(true);
		pageCursor.setImplicitSave(true);
		pageCursor.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				AnnotationsWindow.this.page = Integer.parseInt("" + event.getValue());
				showPage(page);
			}
		});

		toolStrip.addFormItem(pageCursor);

		if (!SC.isIE()) {
			toolStrip.addSeparator();
			toolStrip.addButton(addAnnotation);
		}

		contentPane = new HTMLPane();
		contentPane.setWidth100();
		contentPane.setHeight(getHeight() - 80);
		contentPane.setShowEdges(false);
		contentPane.setContentsType(ContentsType.PAGE);

		pageContent = new VLayout();
		pageContent.setHeight100();
		pageContent.setMembers(toolStrip, contentPane);

		annotations = new VLayout();
		annotations.setHeight100();
		annotations.setWidth(310);
		annotations.setMembersMargin(3);
		annotations.setHeight(getHeight() - 20);
		annotations.setMargin(5);
		// annotations.setShowResizeBar(true);

		frame.setHeight100();
		frame.setWidth100();
		frame.setMembers(annotations, pageContent);

		showPage(1);

		addItem(frame);
	}

	void showPage(int page) {
		if (contentPane != null)
			pageContent.removeMember(contentPane);
		contentPane = new HTMLPane();
		contentPane.setWidth100();
		contentPane.setHeight(getHeight() - 80);
		contentPane.setShowEdges(false);
		contentPane.setContentsType(ContentsType.PAGE);
		NumberFormat nb = NumberFormat.getFormat("0000");

		String contents = "<iframe id='ann-content-panel' src='"
				+ Util.downloadURL(docId, null, nb.format(page) + "-conversion.html", true)
				+ "' style='width:100%;height:100%;border:none'/>";
		if (SC.isIE()) {
			// In case of IE, add the Add Annotation button directly in the HTML
			// contents
			contents = "<button type='button' onclick='onAddAnnotation();' style='margin-top: 5px'>"
					+ I18N.message("addannotation") + "</button><hr />" + contents;
		}

		contentPane.setContents(contents);
		pageContent.addMember(contentPane);

		if (notesGrid != null)
			annotations.removeMember(notesGrid);
		prepareGrid();
		annotations.addMember(notesGrid);
	}

	private void prepareGrid() {
		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField userId = new ListGridField("userId", "userid", 50);
		userId.setHidden(true);

		ListGridField user = new ListGridField("user", I18N.message("author"), 150);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.LEFT);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);

		notesGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
				return new HTMLFlow(
						"<div class='details'>"
								+ (record.getAttributeAsString("message") != null ? record
										.getAttributeAsString("message") : "") + "</div>");
			}
		};
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, docId, page));
		notesGrid.setFields(id, userId, user, date);
		notesGrid.setCanExpandRecords(true);
		notesGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		notesGrid.setDetailField("message");
		notesGrid.setSelectionType(SelectionStyle.SINGLE);
		notesGrid.setWidth100();
		notesGrid.setHeight100();

		// Expand all notes after arrived
		notesGrid.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				for (ListGridRecord rec : notesGrid.getRecords()) {
					notesGrid.expandRecord(rec);
				}
			}
		});

		notesGrid.addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

			@Override
			public void onSelectionUpdated(SelectionUpdatedEvent event) {
				if (!handlingOnOver)
					highlightAnnotation(notesGrid.getSelectedRecord().getAttributeAsString("id"));
			}
		});

		notesGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new Menu();
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						onDelete();
					}
				});

				MenuItem edit = new MenuItem();
				edit.setTitle(I18N.message("edit"));
				edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						AnnotationEditor note = new AnnotationEditor(docId, notesGrid.getSelectedRecord()
								.getAttributeAsLong("id"), null, AnnotationsWindow.this, notesGrid.getSelectedRecord()
								.getAttribute("message"), null, 0);
						note.show();
					}
				});

				MenuItem print = new MenuItem();
				print.setTitle(I18N.message("print"));
				print.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						HTMLPane printContainer = new HTMLPane();
						printContainer.setContents(notesGrid.getSelectedRecord().getAttribute("message"));
						Canvas.showPrintPreview(printContainer);
					}
				});

				ListGridRecord[] selection = notesGrid.getSelectedRecords();

				if (Session.get().getUser().isMemberOf("admin")) {
					delete.setEnabled(selection.length > 0);
					edit.setEnabled(selection.length == 1);
				} else {
					long userId = Long.parseLong(selection[0].getAttribute("userId"));
					delete.setEnabled(selection.length == 1 && userId == Session.get().getUser().getId());
					edit.setEnabled(selection.length == 1 && userId == Session.get().getUser().getId());
				}

				print.setEnabled(selection.length == 1);

				contextMenu.setItems(edit, print, delete);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});

	}

	private void onDelete() {
		ListGridRecord selection = notesGrid.getSelectedRecord();
		final long noteId = Long.parseLong(selection.getAttribute("id"));

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
			@Override
			public void execute(Boolean value) {
				if (value) {
					DocumentService.Instance.get().deleteNotes(new long[] { noteId }, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							notesGrid.removeSelectedData();
							removeAnnotation("" + noteId);
							AnnotationsService.Instance.get().savePage(docId, page, getPageContent(),
									new AsyncCallback<Void>() {

										@Override
										public void onFailure(Throwable caught) {
											Log.serverError(caught);
										}

										@Override
										public void onSuccess(Void arg) {
										}
									});
						}
					});
				}
			}
		});
	}

	@Override
	protected void onInit() {
		declareAPI(AnnotationsWindow.this);
	}

	public void onUpdated(String message) {
		ListGridRecord record = notesGrid.getSelectedRecord();
		record.setAttribute("username", Session.get().getUser().getFullName());
		record.setAttribute("date", new Date());
		record.setAttribute("message", message);
		notesGrid.refreshRow(notesGrid.getRecordIndex(record));
		notesGrid.collapseRecord(record);
		notesGrid.expandRecord(record);
	}

	public void onAddAnnotation() {
		final AnnotationEditor editor = new AnnotationEditor(docId, null, null, AnnotationsWindow.this, null,
				getSelectedText(), page);
		editor.show();
	}

	public void onMouseOverAnnotation(String annotationId) {
		handlingOnOver = true;
		try {
			ListGridRecord[] records = notesGrid.getRecords();
			if (records == null)
				return;
			notesGrid.deselectAllRecords();
			for (ListGridRecord record : records)
				if (annotationId.equals(record.getAttributeAsString("id"))) {
					notesGrid.selectSingleRecord(record);
					break;
				}
		} finally {
			handlingOnOver = false;
		}
	}

	/**
	 * Applies the annotation to the current selection
	 * 
	 * @param annotationId
	 */
	public native void addTempAnnotationToHTML()/*-{
		$wnd.onAddAnnotation();
	}-*/;

	/*****************************************************************
	 * Mini JavaScript API to handle annotations in the content pane *
	 *****************************************************************/
	public static native void declareAPI(AnnotationsWindow annWindow)/*-{

		$wnd.onAddAnnotation = function() {
			var userSelection = $wnd.annGetContent().getSelection().getRangeAt(
					0);
			var safeRanges = $wnd.annGetSafeRanges(userSelection);

			for (var i = 0; i < safeRanges.length; i++) {
				$wnd.annAddAnnotationInRange(safeRanges[i], 'newannotationid');
			}

			annWindow.@com.logicaldoc.gui.frontend.client.annnotation.AnnotationsWindow::onAddAnnotation()();
		};

		$wnd.onMouseOverAnnotation = function(annotationId) {
			annWindow.@com.logicaldoc.gui.frontend.client.annnotation.AnnotationsWindow::onMouseOverAnnotation(Ljava/lang/String;)(annotationId);
		};

		$wnd.annGetContent = function() {
			return $doc.getElementById("ann-content-panel").contentWindow;
		};

		$wnd.annGetElementsByAnnotationId = function annGetElementsByAnnotationId(
				annotationId) {
			var matchingElements = [];
			var allElements = $wnd.annGetContent().document
					.getElementsByTagName('span');
			for (var i = 0; i < allElements.length; i++) {
				if (annotationId == allElements[i].getAttribute('annotationid'))
					matchingElements.push(allElements[i]);
			}
			return matchingElements;
		}

		$wnd.annHighlight = function annHighlight(annotationId) {
			$wnd.annRemoveAllHighlights();
			var elements = [];
			elements = $wnd.annGetElementsByAnnotationId(annotationId);
			for (var i = 0, n = elements.length; i < n; i++) {
				var obj = elements[i];
				obj.style.border = '1px solid red';
			}

			setTimeout(function() {
				$wnd.annRemoveAllHighlights();
			}, 3000);
		}

		$wnd.annRemoveAllHighlights = function annRemoveAllHighlights() {
			var allElements = $wnd.annGetContent().document
					.getElementsByTagName('span');
			for (var i = 0; i < allElements.length; i++) {
				if (allElements[i].getAttribute('annotationid') != null)
					allElements[i].removeAttribute('style');
			}
		}

		$wnd.annGetSafeRanges = function(dangerous) {
			var a = dangerous.commonAncestorContainer;
			// Starts -- Work inward from the start, selecting the largest safe range
			var s = new Array(0), rs = new Array(0);
			if (dangerous.startContainer != a)
				for (var i = dangerous.startContainer; i != a; i = i.parentNode)
					s.push(i);

			if (0 < s.length)
				for (var i = 0; i < s.length; i++) {
					var xs = document.createRange();
					if (i) {
						xs.setStartAfter(s[i - 1]);
						xs.setEndAfter(s[i].lastChild);
					} else {
						xs.setStart(s[i], dangerous.startOffset);
						xs.setEndAfter((s[i].nodeType == Node.TEXT_NODE) ? s[i]
								: s[i].lastChild);
					}
					rs.push(xs);
				}

			// Ends -- basically the same code reversed
			var e = new Array(0), re = new Array(0);
			if (dangerous.endContainer != a)
				for (var i = dangerous.endContainer; i != a; i = i.parentNode)
					e.push(i);

			if (0 < e.length)
				for (var i = 0; i < e.length; i++) {
					var xe = document.createRange();
					if (i) {
						xe.setStartBefore(e[i].firstChild);
						xe.setEndBefore(e[i - 1]);
					} else {
						xe
								.setStartBefore((e[i].nodeType == Node.TEXT_NODE) ? e[i]
										: e[i].firstChild);
						xe.setEnd(e[i], dangerous.endOffset);
					}
					re.unshift(xe);
				}

			// Middle -- the uncaptured middle
			if ((0 < s.length) && (0 < e.length)) {
				var xm = document.createRange();
				xm.setStartAfter(s[s.length - 1]);
				xm.setEndBefore(e[e.length - 1]);
			} else {
				return [ dangerous ];
			}

			// Concat
			rs.push(xm);
			response = rs.concat(re);

			// Send to Console
			return response;
		}

		$wnd.annAddAnnotationInRange = function annAddAnnotationInRange(range,
				annotationId) {
			$wnd.annRemoveAllHighlights();
			var newNode = $wnd.annGetContent().document.createElement("span");
			newNode.className = 'ann-highlight';
			newNode.setAttribute("annotationid", annotationId);
			newNode.setAttribute("onmouseover",
					"window.parent.onMouseOverAnnotation('" + annotationId
							+ "');");
			range.surroundContents(newNode);
		}

		$wnd.annRemoveAnnotation = function annRemoveAnnotation(annotationId) {
			$wnd.annRemoveAllHighlights();

			var allElements = $wnd.annGetContent().document
					.getElementsByTagName('span');

			for (var i = 0; i < allElements.length; i++) {
				var spanElement = allElements[i];
				var annId = spanElement.getAttribute("annotationid");
				var entered = false;
				if (annId != null && annId == annotationId) {
					entered = true;
					var newParent = spanElement.parentNode;

					while (spanElement.childNodes.length > 0) {
						newParent.insertBefore(spanElement.childNodes[0],
								spanElement);
					}
					spanElement.remove();
				}
				if (entered)
					$wnd.annRemoveAnnotation(annotationId);
			}
		}

		$wnd.annUpdateAnnotation = function annUpdateAnnotation(annotationId,
				newAnnotationId) {
			$wnd.annRemoveAllHighlights();
			var allElements = $wnd.annGetContent().document
					.getElementsByTagName('span');

			for (var i = 0; i < allElements.length; i++) {
				var spanElement = allElements[i];
				var annId = spanElement.getAttribute("annotationid");
				if (annId != null && annId == annotationId) {
					spanElement.setAttribute("annotationid", newAnnotationId);
					spanElement.setAttribute("onmouseover",
							"window.parent.onMouseOverAnnotation('"
									+ newAnnotationId + "');");
				}
			}
		}

	}-*/;

	public native void highlightAnnotation(String annotationId)/*-{
		$wnd.annHighlight(annotationId);
	}-*/;

	public native void removeAnnotation(String annotationId)/*-{
		$wnd.annRemoveAnnotation(annotationId);
	}-*/;

	/**
	 * Retrieves the text selected by the user to put an annotation
	 */
	public native String getSelectedText()/*-{
		var userSelection = $wnd.annGetContent().getSelection().getRangeAt(0);
		var safeRanges = $wnd.annGetSafeRanges(userSelection);

		var text = "";
		for (var i = 0; i < safeRanges.length; i++)
			text += safeRanges[i];
		return text
	}-*/;

	/**
	 * Gets the full HTML of the current page.
	 */
	public native String getPageContent()/*-{
		var markup = $wnd.annGetContent().document.documentElement.innerHTML;
		return markup;
	}-*/;
}