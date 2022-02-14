package com.logicaldoc.gui.frontend.client.calendar;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIReminder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used for editing a calendar event.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class CalendarEventDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private GUICalendarEvent calendarEvent;

	private TabSet tabs = new TabSet();

	private DynamicForm detailsForm = new DynamicForm();

	private boolean readOnly = false;

	private ListGrid remindersGrid;

	private AsyncCallback<Void> onChangedCallback;

	public CalendarEventDialog(GUICalendarEvent calEvent, AsyncCallback<Void> onChangedCallback) {
		this.calendarEvent = calEvent;
		this.onChangedCallback = onChangedCallback;

		readOnly = Session.get().getUser().getId() != calEvent.getCreatorId()
				&& !Session.get().getUser().isMemberOf("admin");

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (calEvent.getId() != 0)
			setTitle(I18N.message("editevent") + " - " + calEvent.getTitle());
		else
			setTitle(I18N.message("newevent"));
		setWidth(530);
		setHeight(500);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);

		Tab detailsTab = prepareDetails();
		Tab participantsTab = prepareParticipants();
		Tab documentsTab = prepareDocuments();
		Tab remindersTab = prepareReminders();

		tabs.setTabs(detailsTab, remindersTab, participantsTab, documentsTab);
		tabs.setHeight100();
		addItem(tabs);

		HLayout buttonsPanel = new HLayout();

		IButton save = new IButton();
		save.setMargin(3);
		save.setHeight(30);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		IButton delete = new IButton();
		delete.setMargin(3);
		delete.setHeight(30);
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onDelete();
			}
		});

		if (calendarEvent.getId() != 0)
			buttonsPanel.setMembers(save, delete);
		else
			buttonsPanel.setMembers(save);

		if (!readOnly)
			addItem(buttonsPanel);
	}

	private Tab prepareReminders() {
		ListGridField value = new ListGridField("value", I18N.message("value"), 60);
		value.setType(ListGridFieldType.INTEGER);

		ListGridField unit = new ListGridField("unit", I18N.message("unit"), 100);
		unit.setAutoFitWidth(true);
		unit.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if ("minute".equals(value))
					return I18N.message("minutes").toLowerCase() + " "
							+ I18N.message(record.getAttributeAsString("when")).toLowerCase();
				else if ("hour".equals(value))
					return I18N.message("hours").toLowerCase() + " "
							+ I18N.message(record.getAttributeAsString("when")).toLowerCase();
				else if ("day".equals(value))
					return I18N.message("ddays").toLowerCase() + " "
							+ I18N.message(record.getAttributeAsString("when")).toLowerCase();
				else
					return value != null ? value.toString() : "";
			}
		});

		ListGridField ddelete = new ListGridField("ddelete", I18N.message("delete"));
		ddelete.setWidth("*");
		ddelete.setCanEdit(false);

		remindersGrid = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {
				String fieldName = this.getFieldName(colNum);

				HLayout iconCanvas = new HLayout(3);
				iconCanvas.setHeight(22);
				iconCanvas.setWidth100();
				iconCanvas.setMembersMargin(1);
				iconCanvas.setAlign(Alignment.LEFT);

				if ("ddelete".equals(fieldName)) {
					ToolStripButton deleteIcon = AwesomeFactory.newIconButton("trash-alt", "ddelete");
					deleteIcon.setBaseStyle("statusIcon");
					deleteIcon.setMargin(2);
					deleteIcon.addClickHandler(new ClickHandler() {

						@Override
						public void onClick(ClickEvent event) {
							LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
								@Override
								public void execute(Boolean value) {
									if (value) {
										remindersGrid.removeData(record);
									}
								}
							});
						}
					});

					ToolStripButton addIcon = AwesomeFactory.newIconButton("plus", "add");
					addIcon.setBaseStyle("statusIcon");
					addIcon.setMargin(2);
					addIcon.addClickHandler(new ClickHandler() {

						@Override
						public void onClick(ClickEvent event) {
							addNewReminder();
						}
					});

					iconCanvas.setMembers(deleteIcon, addIcon);
					return iconCanvas;
				} else
					return null;
			}
		};

		remindersGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		remindersGrid.setWidth100();
		remindersGrid.setHeight100();
		remindersGrid.setAutoFetchData(true);
		remindersGrid.setCanSelectAll(false);
		remindersGrid.setSelectionType(SelectionStyle.SINGLE);
		remindersGrid.setShowHeader(false);
		remindersGrid.setCanEdit(!readOnly);
		remindersGrid.setAutoConfirmSaveEdits(true);
		remindersGrid.setEditByCell(true);
		remindersGrid.setCanReorderRecords(true);
		remindersGrid.setShowRecordComponents(true);
		remindersGrid.setShowRecordComponentsByCell(true);
		remindersGrid.setFields(value, unit, ddelete);

		remindersGrid.setEditorCustomizer(new ListGridEditorCustomizer() {
			public FormItem getEditor(ListGridEditorContext context) {
				ListGridField field = context.getEditField();
				if (field.getName().equals("value")) {
					SpinnerItem remindValue = new SpinnerItem();
					remindValue.setDefaultValue(0);
					remindValue.setMin(0);
					remindValue.setStep(1);
					remindValue.setWidth(50);
					remindValue.setRequired(true);
					return remindValue;
				} else if (field.getName().equals("unit")) {
					SelectItem unitSelector = ItemFactory.newDueTimeSelector("remindUnit", "");
					LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
					map.put("minute", I18N.message("minutes"));
					map.put("hour", I18N.message("hours"));
					map.put("day", I18N.message("ddays"));
					unitSelector.setValueMap(map);
					unitSelector.setRequired(true);
					return unitSelector;
				} else
					return null;
			}
		});

		fillRemindersGrid(remindersGrid);

		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setAlign(Alignment.CENTER);
		layout.setMembersMargin(3);

		IButton addReminder = new IButton(I18N.message("newreminder"));
		addReminder.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				addNewReminder();
			}
		});

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.RIGHT);
		buttons.setMembers(addReminder);

		layout.setMembers(remindersGrid, buttons);

		Tab remindersTab = new Tab();
		remindersTab.setTitle(I18N.message("reminders"));
		remindersTab.setPane(layout);
		return remindersTab;
	}

	private void fillRemindersGrid(ListGrid list) {
		ListGridRecord[] records = new ListGridRecord[calendarEvent.getReminders().length];
		for (int i = 0; i < calendarEvent.getReminders().length; i++) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("value", calendarEvent.getReminders()[i].getValue());
			records[i].setAttribute("unit", calendarEvent.getReminders()[i].getUnit());
			records[i].setAttribute("when", "before");
			records[i].setAttribute("date", calendarEvent.getReminders()[i].getDate());
			records[i].setAttribute("reminded", calendarEvent.getReminders()[i].getReminded());
		}
		list.setRecords(records);
	}

	private Tab prepareParticipants() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);
		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth("*");
		ListGridField username = new ListGridField("username", I18N.message("username"));
		username.setWidth(110);

		UserListGridField avatar = new UserListGridField();

		final ListGrid list = new ListGrid();
		list.setHeight100();
		list.setWidth100();
		list.setFields(id, avatar, username, name);

		ListGridRecord[] records = new ListGridRecord[calendarEvent.getParticipants().length];
		for (int i = 0; i < calendarEvent.getParticipants().length; i++) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("id", calendarEvent.getParticipants()[i].getId());
			records[i].setAttribute("avatar", calendarEvent.getParticipants()[i].getId());
			records[i].setAttribute("name", calendarEvent.getParticipants()[i].getFullName());
			records[i].setAttribute("username", calendarEvent.getParticipants()[i].getUsername());
		}
		list.setRecords(records);

		Menu contextMenu = new Menu();
		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message("ddelete"));
		deleteItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;
				for (ListGridRecord rec : selection) {
					if (rec.getAttribute("id").startsWith("g-"))
						CalendarEventDialog.this.calendarEvent
								.removeParticipantGroup(Long.parseLong(rec.getAttribute("id").substring(2)));
					else
						CalendarEventDialog.this.calendarEvent.removeParticipant(rec.getAttributeAsLong("id"));
				}

				list.removeSelectedData();
			}
		});
		if (!readOnly) {
			contextMenu.setItems(deleteItem);
			list.setContextMenu(contextMenu);
		}

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		final SelectItem newUser = ItemFactory.newUserSelector("user", "adduser", null, true, true);
		newUser.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = newUser.getSelectedRecord();
				if (selectedRecord == null)
					return;

				String id = selectedRecord.getAttribute("id");
				String label = selectedRecord.getAttribute("label");
				String username = selectedRecord.getAttribute("username");

				addParticipant(list, id, username, label);
				newUser.clearValue();
			}
		});

		final SelectItem newGroup = ItemFactory.newGroupSelector("group", "addgroup");
		newGroup.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = newGroup.getSelectedRecord();
				if (selectedRecord == null)
					return;

				String id = "g-" + selectedRecord.getAttribute("id");
				String label = I18N.message("group") + ": " + selectedRecord.getAttribute("name");
				String username = selectedRecord.getAttribute("name");

				addParticipant(list, id, username, label);
				newGroup.clearValue();
			}
		});

		form.setItems(newUser, newGroup);

		Tab participantsTab = new Tab();
		participantsTab.setTitle(I18N.message("participants"));
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		if (readOnly)
			layout.setMembers(list);
		else
			layout.setMembers(list, form);
		participantsTab.setPane(layout);
		return participantsTab;
	}

	private Tab prepareDocuments() {
		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setWidth("90%");
		fileName.setCanEdit(!readOnly);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(true);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(fileName, lastModified);
		refreshDocumentsGrid(list);

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				// Detect the selected record
				ListGridRecord selection = list.getSelectedRecord();

				long id = Long.parseLong(selection.getAttribute("id"));

				DocumentService.Instance.get().getById(id, new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught.getMessage(), caught);
					}

					@Override
					public void onSuccess(GUIDocument doc) {
						PreviewPopup iv = new PreviewPopup(doc);
						iv.show();
					}
				});
			}
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				// Detect selected records
				ListGridRecord[] selection = list.getSelectedRecords();
				if (selection != null && selection.length > 0) {
					for (ListGridRecord record : selection) {
						calendarEvent.removeDocument(Long.parseLong(record.getAttribute("id")));
					}
					refreshDocumentsGrid(list);
				}
			}
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = list.getSelectedRecord();
				if (selection != null)
					DocumentsPanel.get().openInFolder(Long.parseLong(selection.getAttributeAsString("folderId")),
							Long.parseLong(selection.getAttributeAsString("id")));
			}
		});

		Menu contextMenu = new Menu();
		if (!readOnly)
			contextMenu.setItems(preview, openInFolder, delete);
		else
			contextMenu.setItems(preview, openInFolder);
		list.setContextMenu(contextMenu);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				destroy();
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(record.getAttributeAsLong("folderId"),
						record.getAttributeAsLong("id"));
			}
		});

		IButton addDocuments = new IButton();
		addDocuments.setTitle(I18N.message("adddocuments"));
		addDocuments.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				Clipboard clipboard = Clipboard.getInstance();
				if (clipboard.isEmpty()) {
					SC.warn(I18N.message("nodocsinclipboard"));
					return;
				}

				for (GUIDocument doc : clipboard) {
					calendarEvent.addDocument(doc);
				}
				Clipboard.getInstance().clear();
				refreshDocumentsGrid(list);
			}
		});

		Tab documentsTab = new Tab();
		documentsTab.setTitle(I18N.message("documents"));
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setAlign(Alignment.CENTER);
		layout.setMembersMargin(3);

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.RIGHT);
		buttons.setMembers(addDocuments);

		if (!readOnly)
			layout.setMembers(list, buttons);
		else
			layout.setMembers(list);

		documentsTab.setPane(layout);
		return documentsTab;
	}

	private void refreshDocumentsGrid(final ListGrid list) {
		ListGridRecord[] records = new ListGridRecord[calendarEvent.getDocuments().length];
		for (int i = 0; i < calendarEvent.getDocuments().length; i++) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("id", calendarEvent.getDocuments()[i].getId());
			records[i].setAttribute("folderId", calendarEvent.getDocuments()[i].getFolder().getId());
			records[i].setAttribute("icon", calendarEvent.getDocuments()[i].getIcon());
			records[i].setAttribute("version", calendarEvent.getDocuments()[i].getVersion());
			records[i].setAttribute("fileVersion", calendarEvent.getDocuments()[i].getFileVersion());
			records[i].setAttribute("filename", calendarEvent.getDocuments()[i].getFileName());
			records[i].setAttribute("lastModified", calendarEvent.getDocuments()[i].getLastModified());
			records[i].setAttribute("docRef", calendarEvent.getDocuments()[i].getDocRef());
			records[i].setAttribute("color", calendarEvent.getDocuments()[i].getColor());
		}
		list.setRecords(records);
	}

	private Tab prepareDetails() {
		Tab details = new Tab();
		details.setTitle(I18N.message("details"));

		final int formColumns = 6;
		detailsForm.setHeight100();
		detailsForm.setTitleOrientation(TitleOrientation.TOP);
		detailsForm.setNumCols(formColumns);
		detailsForm.setValuesManager(vm);

		TextItem title = ItemFactory.newTextItem("title", "title", calendarEvent.getTitle());
		title.setRequired(true);
		title.setEndRow(true);
		title.setWrapTitle(false);
		title.setColSpan(5);
		title.setWidth(350);
		title.setTitleOrientation(TitleOrientation.LEFT);
		title.setLength(255);
		title.setCanEdit(!readOnly);

		TextItem type = ItemFactory.newTextItem("type", "type", calendarEvent.getType());
		type.setRequired(false);
		type.setEndRow(true);
		type.setWrapTitle(false);
		type.setColSpan(5);
		type.setWidth(350);
		type.setTitleOrientation(TitleOrientation.LEFT);
		type.setLength(255);
		type.setCanEdit(!readOnly);

		TextItem subType = ItemFactory.newTextItem("subType", "subtype", calendarEvent.getSubType());
		subType.setRequired(false);
		subType.setEndRow(true);
		subType.setWrapTitle(false);
		subType.setColSpan(5);
		subType.setWidth(350);
		subType.setTitleOrientation(TitleOrientation.LEFT);
		subType.setLength(255);
		subType.setCanEdit(!readOnly);

		DateItem startDate = ItemFactory.newDateItem("startDate", "begin");
		startDate.setRequired(true);
		startDate.setTitleOrientation(TitleOrientation.LEFT);
		startDate.setWrapTitle(false);
		startDate.setValue(calendarEvent.getStartDate());
		startDate.setCanEdit(!readOnly);
		TimeItem startTime = ItemFactory.newTimeItem("startTime", "   ");
		DateTimeFormat df = DateTimeFormat.getFormat("HH:mm");
		startTime.setValue(df.format(calendarEvent.getStartDate()));
		startTime.setRequired(true);
		startTime.setShowTitle(false);
		startTime.setTitleOrientation(TitleOrientation.LEFT);
		startTime.setEndRow(true);
		startTime.setCanEdit(!readOnly);
		startTime.setTitleColSpan(1);

		DateItem expirationDate = ItemFactory.newDateItem("expirationDate", "expirationdate");
		expirationDate.setRequired(false);
		expirationDate.setTitleOrientation(TitleOrientation.LEFT);
		expirationDate.setWrapTitle(false);
		expirationDate.setCanEdit(!readOnly);
		if (calendarEvent.getExpirationDate() != null)
			expirationDate.setValue(calendarEvent.getExpirationDate());
		TimeItem expirationTime = ItemFactory.newTimeItem("expirationTime", "   ");
		expirationTime.setTitleOrientation(TitleOrientation.LEFT);
		expirationTime.setShowTitle(false);
		expirationTime.setEndRow(true);
		expirationTime.setCanEdit(!readOnly);
		expirationTime.setTitleColSpan(1);
		if (calendarEvent.getExpirationDate() != null)
			expirationTime.setValue(df.format(calendarEvent.getExpirationDate()));

		final DateItem deadline = ItemFactory.newDateItem("deadline", "enddate");
		deadline.setRequired(false);
		deadline.setShowTitle(true);
		deadline.setTitleOrientation(TitleOrientation.LEFT);
		deadline.setWrapTitle(false);
		deadline.setCanEdit(!readOnly);
		if (calendarEvent.getDeadline() != null)
			deadline.setValue(calendarEvent.getDeadline());

		final SelectItem frequency = ItemFactory.newFrequencySelector("frequency", "frequency");
		frequency.setTitleOrientation(TitleOrientation.LEFT);
		frequency.setValue(Integer.toString(calendarEvent.getFrequency()));
		frequency.setCanEdit(!readOnly);
		frequency.setWrapTitle(false);
		frequency.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				deadline.setDisabled("0".equals(event.getValue()));
				if ("0".equals(event.getValue()))
					deadline.setValue((Date) null);
			}
		});

		final DateItem completionDate = ItemFactory.newDateItem("completionDate", "completedon");
		completionDate.setRequired(false);
		completionDate.setShowTitle(false);
		completionDate.setTitleOrientation(TitleOrientation.LEFT);
		completionDate.setCanEdit(!readOnly);
		completionDate.setDisabled(calendarEvent.getStatus() != GUICalendarEvent.STATUS_COMPLETED);
		if (calendarEvent.getCompletionDate() != null)
			completionDate.setValue(calendarEvent.getCompletionDate());

		SelectItem status = ItemFactory.newCalendarEventStatusSelector("status", "status");
		status.setTitleOrientation(TitleOrientation.LEFT);
		status.setWrapTitle(false);
		status.setValue(Integer.toString(calendarEvent.getStatus()));
		status.setCanEdit(!readOnly);
		status.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				completionDate.setDisabled(!"2".equals(event.getValue()));
				if ("2".equals(event.getValue()))
					completionDate.setValue(new Date());
				else
					completionDate.setValue((Date) null);
			}
		});

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description",
				calendarEvent.getDescription());
		description.setWidth("*");
		description.setHeight("*");
		description.setColSpan(formColumns);
		description.setCanEdit(!readOnly);

		detailsForm.setFields(title, type, subType, ItemFactory.newRowSpacer(), startDate, startTime, expirationDate,
				expirationTime, ItemFactory.newRowSpacer(), frequency, deadline, ItemFactory.newRowSpacer(), status,
				completionDate, ItemFactory.newRowSpacer(), description);
		details.setPane(detailsForm);
		return details;
	}

	/**
	 * Save button handler
	 */
	private void onSave() {
		if (vm.validate()) {
			if (calendarEvent.getDocuments() == null || calendarEvent.getDocuments().length == 0) {
				SC.warn(I18N.message("nodocsincalevent"));
				return;
			}

			calendarEvent.setTitle(vm.getValueAsString("title"));
			calendarEvent.setType(vm.getValueAsString("type"));
			calendarEvent.setSubType(vm.getValueAsString("subType"));
			calendarEvent.setDescription(vm.getValueAsString("description"));

			if (vm.getValue("frequency") != null)
				calendarEvent.setFrequency(Integer.parseInt(vm.getValueAsString("frequency").trim()));

			DateTimeFormat dfDate = DateTimeFormat.getFormat("yyyy-MM-dd");
			DateTimeFormat dfTime = DateTimeFormat.getFormat("HH:mm");
			DateTimeFormat df = DateTimeFormat.getFormat("yyyy-MM-dd HH:mm");

			String str = dfDate.format((Date) vm.getValue("startDate"));
			if (vm.getValue("startTime") != null)
				try {
					calendarEvent.setStartDate(df.parse(str + " " + vm.getValue("startTime").toString()));
				} catch (Throwable t) {
					calendarEvent.setStartDate(df.parse(str + " " + dfTime.format((Date) vm.getValue("startTime"))));
				}

			if (vm.getValue("expirationDate") != null) {
				str = dfDate.format((Date) vm.getValue("expirationDate"));
				if (vm.getValue("expirationTime") != null)
					try {
						calendarEvent.setExpirationDate(df.parse(str + " " + vm.getValue("expirationTime").toString()));
					} catch (Throwable t) {
						calendarEvent.setExpirationDate(
								df.parse(str + " " + dfTime.format((Date) vm.getValue("expirationTime"))));
					}
			}

			if (calendarEvent.getExpirationDate() != null
					&& calendarEvent.getExpirationDate().before(calendarEvent.getStartDate())) {
				SC.warn(I18N.message("endbeforestart"));
				return;
			}

			if (vm.getValue("completionDate") != null)
				calendarEvent.setCompletionDate((Date) vm.getValue("completionDate"));
			else
				calendarEvent.setCompletionDate(null);

			calendarEvent.setStatus(Integer.parseInt(vm.getValue("status").toString()));

			if (vm.getValue("deadline") != null)
				calendarEvent.setDeadline((Date) vm.getValue("deadline"));
			else
				calendarEvent.setDeadline(null);

			saveReminders(calendarEvent);

			CalendarService.Instance.get().saveEvent(calendarEvent, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					destroy();
					if (onChangedCallback != null)
						onChangedCallback.onSuccess(arg);
				}
			});
		}
	}

	private void saveReminders(GUICalendarEvent calendarEvent) {
		ArrayList<GUIReminder> reminders = new ArrayList<GUIReminder>();
		ListGridRecord[] records = remindersGrid.getRecords();
		if (records != null)
			for (ListGridRecord record : records) {
				GUIReminder reminder = new GUIReminder(record.getAttributeAsInt("value"),
						record.getAttributeAsString("unit"));
				reminder.setDate(record.getAttributeAsDate("date"));
				reminder.setReminded(record.getAttributeAsInt("reminded"));
				reminders.add(reminder);
			}
		calendarEvent.setReminders(reminders.toArray(new GUIReminder[0]));
	}

	/**
	 * Delete button handler
	 */
	private void onDelete() {
		GUIUser currentUser = Session.get().getUser();
		if (currentUser.getId() != calendarEvent.getCreatorId() && !currentUser.isMemberOf("admin")) {
			return;
		}

		LD.ask(I18N.message("delevent"), I18N.message("deleventconfirm"), new BooleanCallback() {
			@Override
			public void execute(Boolean value) {
				if (value.booleanValue()) {
					if (calendarEvent.getParentId() != null) {
						LD.ask(I18N.message("delevent"), I18N.message("douwantdeletealloccurrences"),
								new BooleanCallback() {
									@Override
									public void execute(Boolean value) {
										Long id = value ? calendarEvent.getParentId() : calendarEvent.getId();
										CalendarService.Instance.get().deleteEvent(id, new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												destroy();
											}
										});
									}
								});
					} else
						CalendarService.Instance.get().deleteEvent(calendarEvent.getId(), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								destroy();
								if (onChangedCallback != null)
									onChangedCallback.onSuccess(arg);
							}
						});
				}
			}
		});
	}

	private void addParticipant(final ListGrid list, String id, String username, String name) {
		// Check if the selected user is already present in the list
		ListGridRecord[] records = list.getRecords();
		for (ListGridRecord test : records) {
			if (test.getAttribute("id").equals(id))
				return;
		}

		// Update the table
		ListGridRecord record = new ListGridRecord();

		record.setAttribute("id", id);
		record.setAttribute("name", name);
		record.setAttribute("username", username);
		list.addData(record);

		if (id.startsWith("g-")) {
			GUIGroup group = new GUIGroup();
			group.setId(Long.parseLong(id.substring(2)));
			group.setName(username);
			group.setDescription(name);
			CalendarEventDialog.this.calendarEvent.addParticipant(group);
		} else {
			record.setAttribute("avatar", id);
			GUIUser user = new GUIUser();
			user.setId(Long.parseLong(id));
			user.setUsername(username);
			user.setFirstName(name);
			CalendarEventDialog.this.calendarEvent.addParticipant(user);
		}
	}

	private void addNewReminder() {
		ListGridRecord newRecord = new ListGridRecord();
		ListGridRecord[] records = remindersGrid.getRecords();
		if (records != null && records.length > 0) {
			ListGridRecord lastRecord = records[records.length - 1];
			newRecord.setAttribute("value", lastRecord.getAttributeAsInt("value") + 1);
			newRecord.setAttribute("unit", lastRecord.getAttributeAsString("unit"));
			newRecord.setAttribute("reminded", 0);
			newRecord.setAttribute("when", "before");
		} else {
			newRecord.setAttribute("value", 0);
			newRecord.setAttribute("unit", GUIReminder.TIME_UNIT_MINUTE);
			newRecord.setAttribute("reminded", 0);
			newRecord.setAttribute("when", "before");
		}
		remindersGrid.addData(newRecord);
	}
}