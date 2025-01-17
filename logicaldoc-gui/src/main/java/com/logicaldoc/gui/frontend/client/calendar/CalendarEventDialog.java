package com.logicaldoc.gui.frontend.client.calendar;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIReminder;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.selector.DocumentSelectorDialog;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used for editing a calendar event.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class CalendarEventDialog extends Window {

	private static final String DELEVENT = "delevent";

	private static final String NOTIFY = "notify";

	private static final String EMAIL = "email";

	private static final String START = "start";

	private static final String AUTOMATION = "automation";

	private static final String DEADLINE = "deadline";

	private static final String END_TIME = "endTime";

	private static final String END = "end";

	private static final String START_TIME = "startTime";

	private static final String FOLDER_ID = "folderId";

	private static final String REMINDED = "reminded";

	private static final String BEFORE = "before";

	private static final String VALUE = "value";

	private static final String DDELETE = "ddelete";

	private ValuesManager vm = new ValuesManager();

	private GUICalendarEvent calendarEvent;

	private TabSet tabs = new TabSet();

	private DynamicForm detailsForm = new DynamicForm();

	private boolean readOnly = false;

	private ListGrid remindersGrid;

	private ListGrid attendeesGrid;

	private AsyncCallback<Void> onChangedCallback;

	public CalendarEventDialog(GUICalendarEvent calendarEvent, AsyncCallback<Void> onChangedCallback) {
		this.calendarEvent = calendarEvent;
		this.onChangedCallback = onChangedCallback;

		readOnly = Session.get().getUser().getId() != calendarEvent.getOrganizerId()
				&& !Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (calendarEvent.getId() != 0)
			setTitle(I18N.message("editevent") + " - " + calendarEvent.getTitle());
		else
			setTitle(I18N.message("newevent"));
		setWidth(700);
		setHeight(500);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);

		Tab detailsTab = prepareDetails();
		Tab attendeesTab = prepareAttendees();
		Tab documentsTab = prepareDocumentsTab();
		Tab remindersTab = prepareReminders();

		if (Feature.enabled(Feature.AUTOMATION)) {
			Tab automationTab = prepareAutomation();
			tabs.setTabs(detailsTab, remindersTab, attendeesTab, documentsTab, automationTab);
		} else
			tabs.setTabs(detailsTab, remindersTab, attendeesTab, documentsTab);
		tabs.setHeight100();
		addItem(tabs);

		ToolStrip buttonsBar = new ToolStrip();
		buttonsBar.setWidth100();

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());
		buttonsBar.addButton(save);

		ToolStripButton delete = new ToolStripButton(I18N.message(DDELETE));
		delete.addClickHandler(event -> onDelete());
		if (calendarEvent.getId() != 0)
			buttonsBar.addButton(delete);

		CheckboxItem iCalendar = ItemFactory.newCheckbox("icalendar", I18N.message("notifyicalendar"));
		iCalendar.setValue(calendarEvent.isiCalendar());
		iCalendar.addChangedHandler(changed -> calendarEvent.setiCalendar(iCalendar.getValueAsBoolean()));
		buttonsBar.addFormItem(iCalendar);

		buttonsBar.addFill();

		if (!readOnly)
			addItem(buttonsBar);
	}

	private Tab prepareReminders() {
		prepareRemindersGrid();

		fillRemindersGrid();

		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setAlign(Alignment.CENTER);
		layout.setMembersMargin(3);

		IButton addReminder = new IButton(I18N.message("newreminder"));
		addReminder.addClickHandler(event -> addNewReminder());

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.RIGHT);
		buttons.setMembers(addReminder);

		layout.setMembers(remindersGrid, buttons);

		Tab remindersTab = new Tab();
		remindersTab.setTitle(I18N.message("reminders"));
		remindersTab.setPane(layout);
		return remindersTab;
	}

	private void prepareRemindersGrid() {
		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE), 60);
		value.setType(ListGridFieldType.INTEGER);

		ListGridField unit = prepareReminderUnitField();

		ListGridField ddelete = new ListGridField(DDELETE, I18N.message("delete"));
		ddelete.setWidth("*");
		ddelete.setCanEdit(false);

		remindersGrid = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord rec, Integer colNum) {
				String fieldName = this.getFieldName(colNum);

				HLayout iconCanvas = new HLayout(3);
				iconCanvas.setHeight(22);
				iconCanvas.setWidth100();
				iconCanvas.setMembersMargin(1);
				iconCanvas.setAlign(Alignment.LEFT);

				if (DDELETE.equals(fieldName)) {
					Button deleteIcon = AwesomeFactory.newIconButton("trash-alt", DDELETE);
					deleteIcon.setBaseStyle("statusIcon");
					deleteIcon.setMargin(2);
					deleteIcon.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"),
							(Boolean answer) -> {
								if (Boolean.TRUE.equals(answer)) {
									remindersGrid.removeData(rec);
								}
							}));

					Button addIcon = AwesomeFactory.newIconButton("plus", "add");
					addIcon.setBaseStyle("statusIcon");
					addIcon.setMargin(2);
					addIcon.addClickHandler(event -> addNewReminder());

					iconCanvas.setMembers(deleteIcon, addIcon);
					return iconCanvas;
				} else
					return null;
			}
		};

		remindersGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		remindersGrid.setWidth100();
		remindersGrid.setHeight100();
		remindersGrid.setShowAllRecords(true);
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

		remindersGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();
			if (field.getName().equals(VALUE)) {
				SpinnerItem remindValue = new SpinnerItem();
				remindValue.setDefaultValue(0);
				remindValue.setMin(0);
				remindValue.setStep(1);
				remindValue.setWidth(50);
				remindValue.setRequired(true);
				return remindValue;
			} else if (field.getName().equals("unit")) {
				SelectItem unitSelector = ItemFactory.newDueTimeSelector("remindUnit", "");
				LinkedHashMap<String, String> map = new LinkedHashMap<>();
				map.put("minute", I18N.message("minutes"));
				map.put("hour", I18N.message("hours"));
				map.put("day", I18N.message("ddays"));
				unitSelector.setValueMap(map);
				unitSelector.setRequired(true);
				return unitSelector;
			} else
				return null;
		});
	}

	private ListGridField prepareReminderUnitField() {
		ListGridField unit = new ListGridField("unit", I18N.message("unit"), 100);
		unit.setAutoFitWidth(true);
		unit.setCellFormatter((Object cellValue, ListGridRecord rec, int rowNum, int colNum) -> {
			if ("minute".equals(cellValue))
				return I18N.message("minutes").toLowerCase() + " "
						+ I18N.message(rec.getAttributeAsString("when")).toLowerCase();
			else if ("hour".equals(cellValue))
				return I18N.message("hours").toLowerCase() + " "
						+ I18N.message(rec.getAttributeAsString("when")).toLowerCase();
			else if ("day".equals(cellValue))
				return I18N.message("ddays").toLowerCase() + " "
						+ I18N.message(rec.getAttributeAsString("when")).toLowerCase();
			else
				return cellValue != null ? cellValue.toString() : "";
		});
		return unit;
	}

	private void fillRemindersGrid() {
		List<ListGridRecord> records = new ArrayList<>();
		for (GUIReminder reminder : calendarEvent.getReminders()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(VALUE, reminder.getValue());
			rec.setAttribute("unit", reminder.getUnit());
			rec.setAttribute("when", BEFORE);
			rec.setAttribute("date", reminder.getDate());
			rec.setAttribute(REMINDED, reminder.getReminded());
			records.add(rec);
		}
		remindersGrid.setData(records.toArray(new ListGridRecord[0]));
	}

	private Tab prepareAttendees() {
		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth(90);
		id.setHidden(true);
		id.setCanEdit(false);

		ListGridField name = prepareNameField();

		ListGridField email = prepareEmailField();

		ListGridField notify = new ListGridField(NOTIFY, I18N.message(NOTIFY));
		notify.setType(ListGridFieldType.BOOLEAN);
		notify.setWidth(70);

		UserListGridField avatar = new UserListGridField();

		attendeesGrid = new ListGrid();
		attendeesGrid.setShowAllRecords(true);
		attendeesGrid.setHeight100();
		attendeesGrid.setWidth100();
		attendeesGrid.setCanEdit(true);
		attendeesGrid.setEditByCell(true);
		attendeesGrid.setFields(id, avatar, name, email, notify);

		fillAttandeesGrid(attendeesGrid);

		prepareAttendeesContextMenu(attendeesGrid);

		DynamicForm form = prepareAttendeeForm();

		Tab attendeesTab = new Tab();
		attendeesTab.setTitle(I18N.message("attendees"));
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		if (readOnly)
			layout.setMembers(attendeesGrid);
		else
			layout.setMembers(attendeesGrid, form);
		attendeesTab.setPane(layout);
		return attendeesTab;
	}

	private DynamicForm prepareAttendeeForm() {
		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(6);
		final SelectItem newUser = ItemFactory.newUserSelector("user", "adduser", null, true, true, false);
		newUser.addChangedHandler(changed -> {
			ListGridRecord selectedRecord = newUser.getSelectedRecord();
			if (selectedRecord == null)
				return;

			String idValue = selectedRecord.getAttribute("id");
			String labelValue = selectedRecord.getAttribute("label");
			String emailValue = selectedRecord.getAttribute(EMAIL);

			addAttendee(attendeesGrid, idValue, labelValue, emailValue);
			newUser.clearValue();
		});

		final SelectItem newGroup = ItemFactory.newGroupSelector("group", "addgroup");
		newGroup.addChangedHandler(changed -> {
			ListGridRecord selectedRecord = newGroup.getSelectedRecord();
			if (selectedRecord == null)
				return;

			String idValue = "g-" + selectedRecord.getAttribute("id");
			String labelValue = I18N.message("group") + ": " + selectedRecord.getAttribute("name");

			addAttendee(attendeesGrid, idValue, labelValue, "");
			newGroup.clearValue();
		});

		final ComboBoxItem newAttendee = ItemFactory.newEmailComboSelector("attendee", I18N.message("addattendee"));
		newAttendee.addChangedHandler(changed -> {
			ListGridRecord selection = newAttendee.getSelectedRecord();
			if (selection == null)
				return;

			String fullName = "";
			String firstName = selection.getAttributeAsString("firstName");
			String lastName = selection.getAttributeAsString("lastName");
			fullName = (firstName != null ? firstName : "") + " " + (lastName != null ? lastName : "");
			addAttendee(attendeesGrid, "0", fullName, changed.getValue().toString());
			newAttendee.clearValue();
		});
		newAttendee.addKeyPressHandler(keypress -> {
			if ("enter".equalsIgnoreCase(keypress.getKeyName())) {
				addAttendee(attendeesGrid, "0", "", newAttendee.getValue().toString());
				newAttendee.clearValue();
			}
		});

		form.setItems(newUser, newGroup, newAttendee);
		return form;
	}

	private ListGridField prepareEmailField() {
		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL));
		email.setWidth("*");
		email.addEditorExitHandler(exit -> {
			if (!"0".equals(exit.getRecord().getAttributeAsString("id")))
				exit.getGrid().cancelEditing();
		});
		return email;
	}

	private ListGridField prepareNameField() {
		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(110);
		name.addEditorExitHandler(exit -> {
			if (!"0".equals(exit.getRecord().getAttributeAsString("id")))
				exit.getGrid().cancelEditing();
		});
		name.setCellFormatter((value, rec, rowNum, colNum) -> {
			String val = value != null ? value.toString() : "";
			if (rec.getAttributeAsString("id").equals(Long.toString(calendarEvent.getOrganizerId())))
				return val + " (" + I18N.message("organizer") + ")";
			else
				return val;
		});
		return name;
	}

	private void prepareAttendeesContextMenu(final ListGrid attendeesGrid) {
		Menu contextMenu = new Menu();
		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message(DDELETE));
		deleteItem.addClickHandler(click -> attendeesGrid.removeSelectedData());
		if (!readOnly) {
			contextMenu.setItems(deleteItem);
			attendeesGrid.setContextMenu(contextMenu);
		}
	}

	private void fillAttandeesGrid(final ListGrid attendeesGrid) {
		List<ListGridRecord> records = new ArrayList<>();
		for (GUIUser attender : calendarEvent.getAttendees()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", attender.getId());
			rec.setAttribute("avatar", attender.getId());
			rec.setAttribute("name", attender.getFullName());
			rec.setAttribute(EMAIL, attender.getEmail());
			rec.setAttribute(NOTIFY, attender.isEnabled());
			records.add(rec);
		}
		attendeesGrid.setData(records.toArray(new ListGridRecord[0]));
	}

	private Tab prepareDocumentsTab() {
		FileNameListGridField fileName = new FileNameListGridField();
		fileName.setWidth("90%");
		fileName.setCanEdit(!readOnly);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");

		final ListGrid documentsGrid = new ListGrid();
		documentsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		documentsGrid.setWidth100();
		documentsGrid.setHeight100();
		documentsGrid.setCanFreezeFields(true);
		documentsGrid.setAutoFetchData(true);
		documentsGrid.setShowHeader(true);
		documentsGrid.setCanSelectAll(false);
		documentsGrid.setShowAllRecords(true);
		documentsGrid.setSelectionType(SelectionStyle.SINGLE);
		documentsGrid.setFields(fileName, lastModified);
		refreshDocumentsGrid(documentsGrid);

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler(event -> {
			ListGridRecord selection = documentsGrid.getSelectedRecord();

			long id = Long.parseLong(selection.getAttribute("id"));

			DocumentService.Instance.get().getById(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIDocument doc) {
					new PreviewPopup(doc).show();
				}
			});
		});
		preview.setEnabled(
				com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.PREVIEW));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message(DDELETE));
		delete.addClickHandler(event -> {
			for (ListGridRecord rec : documentsGrid.getSelectedRecords()) {
				calendarEvent.removeDocument(Long.parseLong(rec.getAttribute("id")));
			}
			refreshDocumentsGrid(documentsGrid);
		});

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> {
			ListGridRecord selection = documentsGrid.getSelectedRecord();
			DocumentsPanel.get().openInFolder(Long.parseLong(selection.getAttributeAsString(FOLDER_ID)),
					Long.parseLong(selection.getAttributeAsString("id")));
		});

		Menu contextMenu = new Menu();
		if (!readOnly)
			contextMenu.setItems(preview, openInFolder, delete);
		else
			contextMenu.setItems(preview, openInFolder);
		documentsGrid.setContextMenu(contextMenu);

		documentsGrid.addCellDoubleClickHandler((CellDoubleClickEvent event) -> {
			destroy();
			ListGridRecord rec = event.getRecord();
			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong(FOLDER_ID), rec.getAttributeAsLong("id"));
		});

		Button addDocuments = new Button(I18N.message("adddocuments"));
		addDocuments.setAutoFit(true);
		addDocuments.addClickHandler(evnt -> new DocumentSelectorDialog() {

			@Override
			protected void onSelection(List<GUIDocument> selection) {
				for (GUIDocument doc : selection)
					calendarEvent.addDocument(doc);
				refreshDocumentsGrid(documentsGrid);
				close();
			}
		}.show());

		Button addDocumentsFromClipboard = new Button(I18N.message("adddocumentsfromclipboard"));
		addDocumentsFromClipboard.setAutoFit(true);
		addDocumentsFromClipboard.addClickHandler(eevnt -> {

			Clipboard clipboard = Clipboard.getInstance();
			if (clipboard.isEmpty()) {
				SC.warn(I18N.message("nodocsinclipboard"));
				return;
			}

			for (

			GUIDocument doc : clipboard) {
				calendarEvent.addDocument(doc);
			}
			clipboard.clear();

			refreshDocumentsGrid(documentsGrid);
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
		buttons.setMembersMargin(4);
		buttons.setMembers(addDocuments, addDocumentsFromClipboard);

		if (!readOnly)
			layout.setMembers(documentsGrid, buttons);
		else
			layout.setMembers(documentsGrid);

		documentsTab.setPane(layout);
		return documentsTab;
	}

	private void refreshDocumentsGrid(final ListGrid list) {
		List<ListGridRecord> records = new ArrayList<>();
		for (GUIDocument document : calendarEvent.getDocuments()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", document.getId());
			rec.setAttribute(FOLDER_ID, document.getFolder().getId());
			rec.setAttribute("icon", document.getIcon());
			rec.setAttribute("version", document.getVersion());
			rec.setAttribute("fileVersion", document.getFileVersion());
			rec.setAttribute("filename", document.getFileName());
			rec.setAttribute("lastModified", document.getLastModified());
			rec.setAttribute("docRef", document.getDocRef());
			rec.setAttribute("color", document.getColor());
			records.add(rec);
		}
		list.setData(records.toArray(new ListGridRecord[0]));
	}

	private Tab prepareDetails() {
		Tab details = new Tab();
		details.setTitle(I18N.message("details"));

		final int formColumns = 6;
		detailsForm.setHeight100();
		detailsForm.setTitleOrientation(TitleOrientation.TOP);
		detailsForm.setNumCols(formColumns);
		detailsForm.setValuesManager(vm);

		TextItem title = ItemFactory.newTextItem("title", calendarEvent.getTitle());
		title.setRequired(true);
		title.setEndRow(true);
		title.setWrapTitle(false);
		title.setColSpan(5);
		title.setWidth(350);
		title.setTitleOrientation(TitleOrientation.LEFT);
		title.setLength(255);
		title.setCanEdit(!readOnly);

		LinkItem relatedLink = ItemFactory.newLinkItem("relatedlink", "relatedlink", calendarEvent.getExternalId(),
				calendarEvent.getExternalUrl(), calendarEvent.getExternalUrl());
		relatedLink.setEndRow(true);
		relatedLink.setWrapTitle(false);
		relatedLink.setColSpan(5);
		relatedLink.setWidth(350);
		relatedLink.setTitleOrientation(TitleOrientation.LEFT);
		relatedLink.setVisible(calendarEvent.getExternalUrl() != null);

		TextItem type = ItemFactory.newTextItem("type", calendarEvent.getType());
		type.setRequired(false);
		type.setEndRow(true);
		type.setWrapTitle(false);
		type.setColSpan(5);
		type.setWidth(350);
		type.setTitleOrientation(TitleOrientation.LEFT);
		type.setLength(255);
		type.setCanEdit(!readOnly);

		TextItem subType = ItemFactory.newTextItem("subtype", calendarEvent.getSubType());
		subType.setRequired(false);
		subType.setEndRow(true);
		subType.setWrapTitle(false);
		subType.setColSpan(5);
		subType.setWidth(350);
		subType.setTitleOrientation(TitleOrientation.LEFT);
		subType.setLength(255);
		subType.setCanEdit(!readOnly);

		TextItem location = ItemFactory.newTextItem("location", calendarEvent.getLocation());
		location.setRequired(false);
		location.setEndRow(true);
		location.setWrapTitle(false);
		location.setColSpan(5);
		location.setWidth(350);
		location.setTitleOrientation(TitleOrientation.LEFT);
		location.setLength(255);
		location.setCanEdit(!readOnly);

		DateItem start = ItemFactory.newDateItem(START);
		start.setRequired(true);
		start.setTitleOrientation(TitleOrientation.LEFT);
		start.setWrapTitle(false);
		start.setValue(calendarEvent.getStart());
		start.setCanEdit(!readOnly);
		TimeItem startTime = ItemFactory.newTimeItem(START_TIME, "   ");
		DateTimeFormat df = DateTimeFormat.getFormat("HH:mm");
		startTime.setValue(df.format(calendarEvent.getStart()));
		startTime.setRequired(true);
		startTime.setShowTitle(false);
		startTime.setTitleOrientation(TitleOrientation.LEFT);
		startTime.setEndRow(true);
		startTime.setCanEdit(!readOnly);
		startTime.setTitleColSpan(1);

		DateItem end = ItemFactory.newDateItem(END);
		end.setRequired(false);
		end.setTitleOrientation(TitleOrientation.LEFT);
		end.setWrapTitle(false);
		end.setCanEdit(!readOnly);
		if (calendarEvent.getEnd() != null)
			end.setValue(calendarEvent.getEnd());
		TimeItem endTime = ItemFactory.newTimeItem(END_TIME, "   ");
		endTime.setTitleOrientation(TitleOrientation.LEFT);
		endTime.setShowTitle(false);
		endTime.setEndRow(true);
		endTime.setCanEdit(!readOnly);
		endTime.setTitleColSpan(1);
		if (calendarEvent.getEnd() != null)
			endTime.setValue(df.format(calendarEvent.getEnd()));

		final DateItem deadline = ItemFactory.newDateItem(DEADLINE, "enddate");
		deadline.setRequired(false);
		deadline.setShowTitle(true);
		deadline.setTitleOrientation(TitleOrientation.LEFT);
		deadline.setWrapTitle(false);
		deadline.setCanEdit(!readOnly);
		if (calendarEvent.getDeadline() != null)
			deadline.setValue(calendarEvent.getDeadline());

		final SelectItem frequency = ItemFactory.newFrequencySelector();
		frequency.setTitleOrientation(TitleOrientation.LEFT);
		frequency.setValue(Integer.toString(calendarEvent.getFrequency()));
		frequency.setCanEdit(!readOnly);
		frequency.setWrapTitle(false);
		frequency.addChangedHandler(changed -> {
			deadline.setDisabled("0".equals(changed.getValue()));
			if ("0".equals(changed.getValue()))
				deadline.setValue((Date) null);
		});

		SelectItem status = ItemFactory.newCalendarEventStatusSelector();
		status.setTitleOrientation(TitleOrientation.LEFT);
		status.setWrapTitle(false);
		status.setValue(Integer.toString(calendarEvent.getStatus()));
		status.setCanEdit(!readOnly);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", calendarEvent.getDescription());
		description.setWidth("*");
		description.setHeight("*");
		description.setColSpan(formColumns);
		description.setCanEdit(!readOnly);

		detailsForm.setFields(title, relatedLink, type, subType, location, ItemFactory.newRowSpacer(), start, startTime,
				end, endTime, ItemFactory.newRowSpacer(), frequency, deadline, ItemFactory.newRowSpacer(), status,
				ItemFactory.newRowSpacer(), description);
		details.setPane(detailsForm);
		return details;
	}

	private Tab prepareAutomation() {
		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation(AUTOMATION, calendarEvent.getAutomation(),
				null, false);
		automation.setCanEdit(!readOnly);
		automation.setShowTitle(false);
		automation.setWidth("*");
		automation.setHeight("*");

		DynamicForm automationForm = new DynamicForm();
		automationForm.setNumCols(1);
		automationForm.setTitleOrientation(TitleOrientation.TOP);
		automationForm.setWidth100();
		automationForm.setHeight100();
		automationForm.setValuesManager(vm);
		automationForm.setFields(automation);

		Tab automationTab = new Tab();
		automationTab.setTitle(I18N.message(AUTOMATION));
		automationTab.setPane(automationForm);
		return automationTab;
	}

	/**
	 * Save button handler
	 */
	private void onSave() {
		if (Boolean.TRUE.equals(vm.validate())) {
			calendarEvent.setTitle(vm.getValueAsString("title"));
			calendarEvent.setType(vm.getValueAsString("type"));
			calendarEvent.setSubType(vm.getValueAsString("subtype"));
			calendarEvent.setLocation(vm.getValueAsString("location"));
			calendarEvent.setDescription(vm.getValueAsString("description"));
			calendarEvent.setAutomation(vm.getValueAsString(AUTOMATION));

			if (vm.getValue("frequency") != null)
				calendarEvent.setFrequency(Integer.parseInt(vm.getValueAsString("frequency").trim()));

			saveDates();

			if (calendarEvent.getEnd() != null && calendarEvent.getEnd().before(calendarEvent.getStart())) {
				SC.warn(I18N.message("endbeforestart"));
				return;
			}

			calendarEvent.setStatus(Integer.parseInt(vm.getValue("status").toString()));

			if (vm.getValue(DEADLINE) != null)
				calendarEvent.setDeadline((Date) vm.getValue(DEADLINE));
			else
				calendarEvent.setDeadline(null);

			saveReminders();

			saveAttendees();

			LD.contactingServer();
			CalendarService.Instance.get().saveEvent(calendarEvent, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void arg) {
					LD.clearPrompt();
					destroy();
					if (onChangedCallback != null)
						onChangedCallback.onSuccess(arg);
				}
			});
		}
	}

	private void saveDates() {
		DateTimeFormat dfDate = DateTimeFormat.getFormat("yyyy-MM-dd");
		DateTimeFormat dfTime = DateTimeFormat.getFormat("HH:mm");
		DateTimeFormat df = DateTimeFormat.getFormat("yyyy-MM-dd HH:mm");

		String str = dfDate.format((Date) vm.getValue(START));
		if (vm.getValue(START_TIME) != null)
			try {
				calendarEvent.setStart(df.parse(str + " " + vm.getValue(START_TIME).toString()));
			} catch (Exception t) {
				calendarEvent.setStart(df.parse(str + " " + dfTime.format((Date) vm.getValue(START_TIME))));
			}

		if (vm.getValue(END) != null) {
			str = dfDate.format((Date) vm.getValue(END));
			if (vm.getValue(END_TIME) != null)
				try {
					calendarEvent.setEnd(df.parse(str + " " + vm.getValue(END_TIME).toString()));
				} catch (Exception t) {
					calendarEvent.setEnd(df.parse(str + " " + dfTime.format((Date) vm.getValue(END_TIME))));
				}
		} else {
			calendarEvent.setEnd(null);
		}
	}

	private void saveReminders() {
		List<GUIReminder> reminders = new ArrayList<>();
		ListGridRecord[] records = remindersGrid.getRecords();
		if (records != null)
			for (ListGridRecord rec : records) {
				GUIReminder reminder = new GUIReminder(rec.getAttributeAsInt(VALUE), rec.getAttributeAsString("unit"));
				reminder.setDate(rec.getAttributeAsDate("date"));
				reminder.setReminded(rec.getAttributeAsInt(REMINDED));
				reminders.add(reminder);
			}
		calendarEvent.setReminders(reminders);
	}

	private void saveAttendees() {
		List<GUIUser> userAttendees = new ArrayList<>();
		List<GUIGroup> groupAttendees = new ArrayList<>();
		ListGridRecord[] records = attendeesGrid.getRecords();
		if (records != null)
			for (ListGridRecord rec : records) {
				if (rec.getAttributeAsString("id").startsWith("g")) {
					GUIGroup attendee = new GUIGroup();
					attendee.setId(Long.parseLong(rec.getAttributeAsString("id").substring(2)));
					attendee.setName(rec.getAttributeAsString("name"));
					groupAttendees.add(attendee);
				} else {
					GUIUser attendee = new GUIUser();
					attendee.setId(rec.getAttributeAsLong("id"));
					attendee.setEmail(rec.getAttributeAsString(EMAIL));
					attendee.setName(rec.getAttributeAsString("name"));
					attendee.setEnabled(rec.getAttributeAsBoolean(NOTIFY));
					userAttendees.add(attendee);
				}
			}
		calendarEvent.setAttendees(userAttendees);
		calendarEvent.setAttendeesGroups(groupAttendees);
	}

	/**
	 * Delete button handler
	 */
	private void onDelete() {
		GUIUser currentUser = Session.get().getUser();
		if (currentUser.getId() != calendarEvent.getOrganizerId() && !currentUser.isMemberOf(Constants.GROUP_ADMIN))
			return;

		LD.ask(I18N.message(DELEVENT), I18N.message("deleventconfirm"), confirmToDelete -> {
			if (Boolean.FALSE.equals(confirmToDelete))
				return;

			if (calendarEvent.getParentId() != null) {
				LD.ask(I18N.message(DELEVENT), I18N.message("douwantdeletealloccurrences"), answer -> {
					Long id = Boolean.TRUE.equals(answer) ? calendarEvent.getParentId() : calendarEvent.getId();
					deleteEvent(id);
				});
			} else {
				deleteEvent(calendarEvent.getId());
			}
		});
	}

	private void deleteEvent(Long id) {
		LD.ask(I18N.message(DELEVENT), I18N.message("askalertcancelation"), answer -> {
			LD.contactingServer();
			CalendarService.Instance.get().deleteEvent(id, Boolean.TRUE.equals(answer), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void arg) {
					LD.clearPrompt();
					destroy();
					if (onChangedCallback != null)
						onChangedCallback.onSuccess(arg);
				}
			});
		});

	}

	private void addAttendee(final ListGrid list, String id, String name, String email) {
		// Check if the selected user is already present in the list
		ListGridRecord[] records = list.getRecords();
		for (ListGridRecord test : records) {
			if (!"0".equals(id) && test.getAttribute("id").equals(id))
				return;
		}

		// Update the table
		ListGridRecord rec = new ListGridRecord();

		if (!id.startsWith("g") && !"0".equals(id))
			rec.setAttribute("avatar", id);
		rec.setAttribute("id", id);
		rec.setAttribute("name", name);
		rec.setAttribute(EMAIL, email);
		rec.setAttribute(NOTIFY, true);
		list.addData(rec);
	}

	private void addNewReminder() {
		ListGridRecord newRecord = new ListGridRecord();
		ListGridRecord[] records = remindersGrid.getRecords();
		if (records != null && records.length > 0) {
			ListGridRecord lastRecord = records[records.length - 1];
			newRecord.setAttribute(VALUE, lastRecord.getAttributeAsInt(VALUE) + 1);
			newRecord.setAttribute("unit", lastRecord.getAttributeAsString("unit"));
			newRecord.setAttribute(REMINDED, 0);
			newRecord.setAttribute("when", BEFORE);
		} else {
			newRecord.setAttribute(VALUE, 0);
			newRecord.setAttribute("unit", GUIReminder.TIME_UNIT_MINUTE);
			newRecord.setAttribute(REMINDED, 0);
			newRecord.setAttribute("when", BEFORE);
		}
		remindersGrid.addData(newRecord);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((calendarEvent == null) ? 0 : calendarEvent.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		CalendarEventDialog other = (CalendarEventDialog) obj;
		if (calendarEvent == null) {
			if (other.calendarEvent != null)
				return false;
		} else if (!calendarEvent.equals(other.calendarEvent))
			return false;
		return true;
	}
}