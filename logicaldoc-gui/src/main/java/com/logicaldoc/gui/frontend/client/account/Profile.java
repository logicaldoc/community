package com.logicaldoc.gui.frontend.client.account;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.Avatar;
import com.logicaldoc.gui.frontend.client.menu.MainMenu;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to change file data of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Profile extends Window {

	private static final String DATEFORMATLONG = "dateformatlong";

	private static final String DATEFORMATSHORT = "dateformatshort";

	private static final String DATEFORMAT = "dateformat";

	private static final String HITSGRIDLAYOUT = "hitsgridlayout";

	private static final String DOCSGRIDLAYOUT = "docsgridlayout";

	private static final String NOTCUSTOMIZED = "notcustomized";

	private static final String SIGNATURE = "signature";

	private static final String EMAIL = "email";

	private static final String TIMEZONE = "timezone";

	private ValuesManager vm = new ValuesManager();

	private ListGrid searchesList;

	public Profile(final GUIUser user) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("profile"));
		setWidth(640);
		setHeight(430);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setCanDragResize(true);

		final DynamicForm detailsForm = new DynamicForm();
		detailsForm.setHeight100();
		detailsForm.setWidth(1);
		detailsForm.setValuesManager(vm);
		detailsForm.setMargin(5);
		detailsForm.setNumCols(3);
		detailsForm.setTitleOrientation(TitleOrientation.TOP);

		TextItem firstName = ItemFactory.newTextItem("firstname", user.getFirstName());
		firstName.setRequired(true);
		TextItem lastName = ItemFactory.newTextItem("lastname", user.getName());
		lastName.setRequired(true);
		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setValue(user.getLanguage());
		language.setDisabled(Session.get().isDemo() && Session.get().getUser().getId() == 1);

		TextItem address = ItemFactory.newTextItem("address", user.getAddress());
		TextItem postalCode = ItemFactory.newTextItem("postalcode", user.getPostalCode());
		TextItem city = ItemFactory.newTextItem("city", user.getCity());
		TextItem country = ItemFactory.newTextItem("country", user.getCountry());
		TextItem state = ItemFactory.newTextItem("state", user.getState());
		TextItem phone = ItemFactory.newTextItem("phone", user.getPhone());
		TextItem cell = ItemFactory.newTextItem("cell", user.getCell());
		TextItem company = ItemFactory.newTextItem("company", user.getCompany());
		TextItem department = ItemFactory.newTextItem("department", user.getDepartment());
		TextItem organizationalUnit = ItemFactory.newTextItem("organizationalunit", user.getOrganizationalUnit());
		TextItem building = ItemFactory.newTextItem("building", user.getBuilding());

		ComboBoxItem timeZone = ItemFactory.newTimeZoneSelector(TIMEZONE, user.getTimeZone());
		timeZone.setEndRow(true);

		StaticTextItem quota = ItemFactory.newStaticTextItem("quota", "maxquota", Util.formatSizeW7(user.getQuota()));
		quota.setWrap(false);

		StaticTextItem quotaCount = ItemFactory.newStaticTextItem("quotaCount", "quota",
				Util.formatSizeW7(user.getQuotaCount()));
		quotaCount.setWrap(false);

		detailsForm.setFields(firstName, lastName, language, address, postalCode, city, country, state, phone, cell,
				company, department, organizationalUnit, building, timeZone, quotaCount, quota);

		HLayout detailsPanel = new HLayout();
		detailsPanel.setMembers(detailsForm, new Avatar(user.getId()));

		final DynamicForm emailForm = new DynamicForm();
		emailForm.setHeight100();
		emailForm.setValuesManager(vm);
		emailForm.setMargin(5);
		emailForm.setTitleOrientation(TitleOrientation.TOP);

		TextItem email = ItemFactory.newEmailItem(EMAIL, EMAIL, false);
		email.setRequired(true);
		email.setWidth(300);
		email.setValue(user.getEmail());

		RichTextItem signature = new RichTextItem();
		signature.setName(SIGNATURE);
		signature.setTitle(I18N.message(SIGNATURE));
		signature.setShowTitle(true);
		signature.setValue(user.getEmailSignature());
		signature.setWidth(getWidth() - 60);
		signature.setHeight(180);

		PickerIcon clearSignature = new PickerIcon(PickerIcon.CLEAR, event -> event.getItem().setValue(""));
		signature.setIcons(clearSignature);

		emailForm.setFields(email, signature);

		final DynamicForm emailForm2 = new DynamicForm();
		emailForm2.setHeight100();
		emailForm2.setValuesManager(vm);
		emailForm2.setMargin(5);
		emailForm2.setTitleOrientation(TitleOrientation.TOP);

		TextItem email2 = ItemFactory.newEmailItem("email2", "secondaryemail", false);
		email2.setWidth(300);
		email2.setValue(user.getEmail2());

		RichTextItem signature2 = new RichTextItem();
		signature2.setName("signature2");
		signature2.setTitle(I18N.message(SIGNATURE));
		signature2.setShowTitle(true);
		signature2.setValue(user.getEmailSignature2());
		signature2.setWidth(getWidth() - 60);
		signature2.setHeight(180);

		PickerIcon clearSignature2 = new PickerIcon(PickerIcon.CLEAR, event -> event.getItem().setValue(""));
		signature2.setIcons(clearSignature2);

		emailForm2.setFields(email2, signature2);

		SelectItem welcomeScreen = ItemFactory.newWelcomeScreenSelector(user.getWelcomeScreen());
		SelectItem defaultWorkspace = ItemFactory.newWorkspaceSelector(user.getDefaultWorkspace());

		PickerIcon clearDocsGrid = new PickerIcon(PickerIcon.CLEAR, event -> {
			event.getItem().setValue(I18N.message(NOTCUSTOMIZED));
			user.setDocsGrid(null);
		});
		clearDocsGrid.setWidth(12);
		clearDocsGrid.setHeight(12);

		PickerIcon clearHitsGrid = new PickerIcon(PickerIcon.CLEAR, event -> {
			event.getItem().setValue(I18N.message(NOTCUSTOMIZED));
			user.setHitsGrid(null);
		});
		clearHitsGrid.setWidth(12);
		clearHitsGrid.setHeight(12);

		FormItemIcon showDocsDefinition = new FormItemIcon();
		showDocsDefinition.setSrc("[SKIN]/paste.gif");
		showDocsDefinition.setPrompt(I18N.message("editlayout"));
		showDocsDefinition.setWidth(12);
		showDocsDefinition.setHeight(12);
		showDocsDefinition.addFormItemClickHandler(event -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem(DOCSGRIDLAYOUT, null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message(DOCSGRIDLAYOUT), I18N.message(DOCSGRIDLAYOUT),
					user.getDocsGrid() != null ? user.getDocsGrid() : "", textArea, 400, user::setDocsGrid);
			event.cancel();
		});

		FormItemIcon showHitsDefinition = new FormItemIcon();
		showHitsDefinition.setSrc("[SKIN]/paste.gif");
		showHitsDefinition.setPrompt(I18N.message("editlayout"));
		showHitsDefinition.setWidth(12);
		showHitsDefinition.setHeight(12);
		showHitsDefinition.addFormItemClickHandler(event -> {
			TextAreaItem textArea = ItemFactory.newTextAreaItem(HITSGRIDLAYOUT, null);
			textArea.setHeight(300);
			LD.askForValue(I18N.message(HITSGRIDLAYOUT), I18N.message(HITSGRIDLAYOUT),
					user.getHitsGrid() != null ? user.getHitsGrid() : "", textArea, 400, user::setHitsGrid);
			event.cancel();
		});

		StaticTextItem docsGrid = ItemFactory.newStaticTextItem("docsgrid", DOCSGRIDLAYOUT,
				user.getDocsGrid() != null && !user.getDocsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message(NOTCUSTOMIZED));
		docsGrid.setIcons(showDocsDefinition, clearDocsGrid);

		StaticTextItem hitsGrid = ItemFactory.newStaticTextItem("hitsgrid", HITSGRIDLAYOUT,
				user.getHitsGrid() != null && !user.getHitsGrid().isEmpty() ? I18N.message("customized")
						: I18N.message(NOTCUSTOMIZED));
		hitsGrid.setIcons(showHitsDefinition, clearHitsGrid);

		TextItem dateFormat = ItemFactory.newTextItem(DATEFORMAT, user.getDateFormat());
		dateFormat.setWidth(180);
		TextItem dateFormatShort = ItemFactory.newTextItem(DATEFORMATSHORT, user.getDateFormatShort());
		dateFormatShort.setWidth(180);
		TextItem dateFormatLong = ItemFactory.newTextItem(DATEFORMATLONG, user.getDateFormatLong());
		dateFormatLong.setWidth(180);

		CheckboxItem evalForm = ItemFactory.newCheckbox("evalformenabled");
		evalForm.setValue(user.isEvalFormEnabled());
		evalForm.setVisible(Menu.enabled(Menu.PRODUCT_EVALUATION));

		final DynamicForm guiForm = new DynamicForm();
		guiForm.setHeight100();
		guiForm.setValuesManager(vm);
		guiForm.setMargin(5);
		guiForm.setTitleOrientation(TitleOrientation.TOP);

		guiForm.setFields(welcomeScreen, defaultWorkspace, docsGrid, hitsGrid, dateFormat, dateFormatShort,
				dateFormatLong, evalForm);

		ArrayList<ListGridRecord> records = new ArrayList<>();
		for (String search : user.orderedSearches()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("search", search);
			rec.setAttribute("label", I18N.message(search));
			records.add(rec);
		}

		ListGridField label = new ListGridField("label", I18N.message("searchpreference"));
		label.setWidth("*");
		label.setAllowFilterOperators(false);
		label.setCanFilter(false);
		label.setCanSort(false);

		searchesList = new ListGrid();
		searchesList.setCanReorderRecords(true);
		searchesList.setCanSort(false);
		searchesList.setCanSort(false);
		searchesList.setCanGroupBy(false);
		searchesList.setCanResizeFields(false);
		searchesList.setSelectionType(SelectionStyle.SINGLE);
		searchesList.setFields(label);
		searchesList.setData(records.toArray(new ListGridRecord[0]));

		HLayout guiLayout = new HLayout();
		guiLayout.setMembers(guiForm, searchesList);

		final TabSet tabs = new TabSet();
		tabs.setHeight100();
		tabs.setWidth100();

		Tab detailsTab = new Tab(I18N.message("details"));
		detailsTab.setPane(detailsPanel);

		Tab guiTab = new Tab(I18N.message("userinterface"));
		guiTab.setPane(guiLayout);

		Tab emailTab = new Tab(I18N.message(EMAIL));
		emailTab.setPane(emailForm);
		Tab emailTab2 = new Tab(I18N.message("secondaryemail"));
		emailTab2.setPane(emailForm2);

		tabs.setTabs(detailsTab, guiTab, emailTab, emailTab2);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> onSave(user, detailsForm, guiForm, emailForm, emailForm2, tabs));
		ToolStrip toolbar = new ToolStrip();
		toolbar.setWidth100();
		toolbar.setMembers(save);

		addItem(toolbar);
		addItem(tabs);
	}

	private void onSave(final GUIUser user, final DynamicForm detailsForm, final DynamicForm guiForm,
			final DynamicForm emailForm, final DynamicForm emailForm2, final TabSet tabs) {
		if (!validate(detailsForm, guiForm, emailForm, emailForm2, tabs))
			return;

		GUIUser u = new GUIUser();
		u.setId(user.getId());
		u.setFirstName(vm.getValueAsString("firstname"));
		u.setName(vm.getValueAsString("lastname"));
		u.setEmail(vm.getValueAsString(EMAIL));
		u.setEmail2(vm.getValueAsString("email2"));
		u.setLanguage(vm.getValueAsString("language"));
		u.setAddress(vm.getValueAsString("address"));
		u.setPostalCode(vm.getValueAsString("postalcode"));
		u.setCity(vm.getValueAsString("city"));
		u.setCountry(vm.getValueAsString("country"));
		u.setState(vm.getValueAsString("state"));
		u.setPhone(vm.getValueAsString("phone"));
		u.setCell(vm.getValueAsString("cell"));
		u.setCompany(vm.getValueAsString("company"));
		u.setDepartment(vm.getValueAsString("department"));
		u.setBuilding(vm.getValueAsString("building"));
		u.setOrganizationalUnit(vm.getValueAsString("organizationalunit"));
		u.setEvalFormEnabled(Boolean.parseBoolean(vm.getValueAsString("evalformenabled")));

		u.setWelcomeScreen(Integer.parseInt(vm.getValueAsString("welcomescreen")));
		String str = vm.getValueAsString("workspace");
		if (str != null && !str.isEmpty())
			u.setDefaultWorkspace(Long.parseLong(str));
		u.setEmailSignature(vm.getValueAsString(SIGNATURE));
		u.setEmailSignature2(vm.getValueAsString("signature2"));
		u.setDocsGrid(user.getDocsGrid());
		u.setHitsGrid(user.getHitsGrid());
		u.setTimeZone(vm.getValueAsString(TIMEZONE));

		if (vm.getValueAsString(DATEFORMAT) == null || vm.getValueAsString(DATEFORMAT).isEmpty())
			u.setDateFormat(null);
		else
			u.setDateFormat(vm.getValueAsString(DATEFORMAT));

		if (vm.getValueAsString(DATEFORMATSHORT) == null || vm.getValueAsString(DATEFORMATSHORT).isEmpty())
			u.setDateFormatShort(null);
		else
			u.setDateFormatShort(vm.getValueAsString(DATEFORMATSHORT));

		if (vm.getValueAsString(DATEFORMATLONG) == null || vm.getValueAsString(DATEFORMATLONG).isEmpty())
			u.setDateFormatLong(null);
		else
			u.setDateFormatLong(vm.getValueAsString(DATEFORMATLONG));

		ListGridRecord[] records = searchesList.getRecords();
		List<String> searches = new ArrayList<>();
		for (ListGridRecord rec : records)
			searches.add(rec.getAttributeAsString("search"));
		u.setSearchPref(searches.toString().replace("[", "").replace("]", "").replace(" ", ""));

		SecurityService.Instance.get().saveProfile(u, new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(GUIUser ret) {
				// Update the currently logged user bean
				user.setFirstName(ret.getFirstName());
				user.setName(ret.getName());
				user.setEmail(ret.getEmail());
				user.setEmailSignature(ret.getEmailSignature());
				user.setEmail2(ret.getEmail2());
				user.setEmailSignature2(ret.getEmailSignature2());
				user.setLanguage(ret.getLanguage());
				user.setAddress(ret.getAddress());
				user.setPostalCode(ret.getPostalCode());
				user.setCity(ret.getCity());
				user.setCountry(ret.getCountry());
				user.setState(ret.getState());
				user.setPhone(ret.getPhone());
				user.setCell(ret.getCell());
				user.setCompany(ret.getCompany());
				user.setBuilding(ret.getBuilding());
				user.setDepartment(ret.getDepartment());
				user.setOrganizationalUnit(ret.getOrganizationalUnit());
				user.setWelcomeScreen(ret.getWelcomeScreen());
				user.setDefaultWorkspace(ret.getDefaultWorkspace());
				user.setDocsGrid(ret.getDocsGrid());
				user.setHitsGrid(ret.getHitsGrid());
				user.setDateFormat(ret.getDateFormat());
				user.setDateFormatShort(ret.getDateFormatShort());
				user.setDateFormatLong(ret.getDateFormatLong());
				user.setSearchPref(ret.getSearchPref());
				user.setTimeZone(ret.getTimeZone());
				user.setEvalFormEnabled(ret.isEvalFormEnabled());

				Session.get().setUser(user);

				Profile.this.destroy();

				GuiLog.info(I18N.message("settingssaved"), null);

				MainMenu.get().refreshProductEvaluationButton();
			}
		});
	}

	private boolean validate(final DynamicForm detailsForm, final DynamicForm guiForm, final DynamicForm emailForm,
			final DynamicForm emailForm2, final TabSet tabs) {
		vm.validate();

		if (!detailsForm.validate())
			tabs.selectTab(0);
		else if (!guiForm.validate())
			tabs.selectTab(2);
		else if (!emailForm.validate())
			tabs.selectTab(2);
		else if (!emailForm2.validate())
			tabs.selectTab(3);

		return !vm.hasErrors();
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