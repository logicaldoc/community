package com.logicaldoc.gui.common.client.util;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.InputValues;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIRetentionPolicy;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.ArchivesDS;
import com.logicaldoc.gui.common.client.data.AttributeOptionsDS;
import com.logicaldoc.gui.common.client.data.AttributeSetsDS;
import com.logicaldoc.gui.common.client.data.AttributesDS;
import com.logicaldoc.gui.common.client.data.AutomationRoutinesDS;
import com.logicaldoc.gui.common.client.data.BarcodeTemplatesDS;
import com.logicaldoc.gui.common.client.data.CharsetsDS;
import com.logicaldoc.gui.common.client.data.ComparatorsDS;
import com.logicaldoc.gui.common.client.data.ContactsDS;
import com.logicaldoc.gui.common.client.data.ConversionFormatsDS;
import com.logicaldoc.gui.common.client.data.DashletsDS;
import com.logicaldoc.gui.common.client.data.EventsDS;
import com.logicaldoc.gui.common.client.data.FolderTemplatesDS;
import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.data.FormsDS;
import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.data.JobsDS;
import com.logicaldoc.gui.common.client.data.OCRTemplatesDS;
import com.logicaldoc.gui.common.client.data.SkinsDS;
import com.logicaldoc.gui.common.client.data.StampsDS;
import com.logicaldoc.gui.common.client.data.StoragesDS;
import com.logicaldoc.gui.common.client.data.StoragesTypesDS;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.data.TemplatesDS;
import com.logicaldoc.gui.common.client.data.TenantsDS;
import com.logicaldoc.gui.common.client.data.TimeZonesDS;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.data.WorkflowsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.validators.EmailValidator;
import com.logicaldoc.gui.common.client.validators.EmailsValidator;
import com.logicaldoc.gui.common.client.validators.SimpleTextValidator;
import com.logicaldoc.gui.common.client.widgets.CronExpressionComposer;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.PasswordGenerator;
import com.logicaldoc.gui.common.client.widgets.UserSelector;
import com.logicaldoc.gui.common.client.widgets.automation.AutomationItemEditor;
import com.logicaldoc.gui.common.client.widgets.automation.HtmlItemEditor;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.MultiComboBoxLayoutStyle;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.DateChooser;
import com.smartgwt.client.widgets.DateRangeDialog;
import com.smartgwt.client.widgets.HeaderControl.HeaderIcon;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemValueFormatter;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.DateRangeItem;
import com.smartgwt.client.widgets.form.fields.FloatItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.MiniDateRangeItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SliderItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.TimeItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.EditorEnterEvent;
import com.smartgwt.client.widgets.form.fields.events.EditorEnterHandler;
import com.smartgwt.client.widgets.form.fields.events.EditorExitEvent;
import com.smartgwt.client.widgets.form.fields.events.EditorExitHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsFloatValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Collection of useful factory methods for form items.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ItemFactory {

	static {
		DateRangeDialog dialog = new DateRangeDialog();
		dialog.setCancelButtonTitle(I18N.message("cancel"));
		dialog.setClearButtonTitle(I18N.message("clear"));
		dialog.setOkButtonTitle(I18N.message("ok"));
		dialog.setHeaderTitle(I18N.message("selectdaterange"));
		dialog.setTitle(I18N.message("selectdaterange"));
		DateRangeDialog.setDefaultProperties(dialog);

		DateRangeItem dateRangeItem = new DateRangeItem();
		dateRangeItem.setFromTitle(I18N.message("from"));
		dateRangeItem.setToTitle(I18N.message("to"));
		dateRangeItem.setDateFormatter(I18N.getDateDisplayFormat(false));
		dateRangeItem.setPickerIconPrompt(I18N.message("showdatechooser"));
		dateRangeItem.setHintStyle("hint");
		dateRangeItem.setRequiredMessage(I18N.message("fieldrequired"));
		dateRangeItem.setBrowserSpellCheck(false);
		DateRangeItem.setDefaultProperties(dateRangeItem);

		MiniDateRangeItem miniDateRangeItem = new MiniDateRangeItem();
		miniDateRangeItem.setShowPickerIcon(true);
		miniDateRangeItem.setAllowRelativeDates(false);
		miniDateRangeItem.setHintStyle("hint");
		miniDateRangeItem.setWidth(180);
		miniDateRangeItem.setDateFormatter(I18N.getDateDisplayFormat(false));
		miniDateRangeItem.setDateDisplayFormat(I18N.getDateDisplayFormat(false));
		miniDateRangeItem.setToDateOnlyPrefix(I18N.message("bbefore"));
		miniDateRangeItem.setFromDateOnlyPrefix(I18N.message("ssince"));
		miniDateRangeItem.setPickerIconPrompt(I18N.message("showdatechooser"));
		miniDateRangeItem.setRequiredMessage(I18N.message("fieldrequired"));
		miniDateRangeItem.setBrowserSpellCheck(false);
		MiniDateRangeItem.setDefaultProperties(miniDateRangeItem);

		DateItem dateItem = new DateItem();
		dateItem.setUseTextField(true);
		dateItem.setUseMask(true);
		dateItem.setShowPickerIcon(true);
		dateItem.setHintStyle("hint");
		dateItem.setWidth(150);
		dateItem.setDateFormatter(I18N.getDateDisplayFormat(false));
		dateItem.setPickerIconPrompt(I18N.message("showdatechooser"));
		dateItem.setRequiredMessage(I18N.message("fieldrequired"));
		dateItem.setEndDate(new Date(new Date().getTime() + 2208992400000L));
		dateItem.setStartDate(new Date(-2208992400000L));
		dateItem.setBrowserSpellCheck(false);
		DateItem.setDefaultProperties(dateItem);

		DateChooser dateChooser = new DateChooser();
		dateChooser.setEndYear(2040);
		dateChooser.setStartYear(1990);
		DateChooser.setDefaultProperties(dateChooser);

		SelectItem selectItem = new SelectItem();
		selectItem.setWidth(150);
		selectItem.setHintStyle("hint");
		selectItem.setRequiredMessage(I18N.message("fieldrequired"));
		selectItem.setBrowserSpellCheck(false);
		SelectItem.setDefaultProperties(selectItem);

		TextItem textItem = new TextItem();
		textItem.setHintStyle("hint");
		textItem.setWidth(150);
		textItem.setRequiredMessage(I18N.message("fieldrequired"));
		textItem.setBrowserSpellCheck(false);
		TextItem.setDefaultProperties(textItem);

		RadioGroupItem radioGroupItem = new RadioGroupItem();
		radioGroupItem.setHintStyle("hint");
		radioGroupItem.setRequiredMessage(I18N.message("fieldrequired"));
		radioGroupItem.setBrowserSpellCheck(false);
		RadioGroupItem.setDefaultProperties(radioGroupItem);

		CheckboxItem checkboxItem = new CheckboxItem();
		checkboxItem.setHintStyle("hint");
		checkboxItem.setRequiredMessage(I18N.message("fieldrequired"));
		checkboxItem.setBrowserSpellCheck(false);
		CheckboxItem.setDefaultProperties(checkboxItem);

		MultiComboBoxItem multiComboBoxItem = new MultiComboBoxItem();
		multiComboBoxItem.setHintStyle("hint");
		multiComboBoxItem.setRequiredMessage(I18N.message("fieldrequired"));
		multiComboBoxItem.setBrowserSpellCheck(false);
		MultiComboBoxItem.setDefaultProperties(multiComboBoxItem);

		SpinnerItem spinnerItem = new SpinnerItem();
		spinnerItem.setHintStyle("hint");
		spinnerItem.setWidth(60);
		spinnerItem.setRequiredMessage(I18N.message("fieldrequired"));
		spinnerItem.setBrowserSpellCheck(false);
		SpinnerItem.setDefaultProperties(spinnerItem);

		PasswordItem passwordItem = new PasswordItem();
		passwordItem.setHintStyle("hint");
		passwordItem.setRequiredMessage(I18N.message("fieldrequired"));
		TextItem.setDefaultProperties(passwordItem);

		StaticTextItem staticTextItem = new StaticTextItem();
		staticTextItem.setWidth(150);
		staticTextItem.setHintStyle("hint");
		staticTextItem.setRequiredMessage(I18N.message("fieldrequired"));
		staticTextItem.setBrowserSpellCheck(false);
		StaticTextItem.setDefaultProperties(staticTextItem);

		IntegerItem integerItem = new IntegerItem();
		integerItem.setRequiredMessage(I18N.message("fieldrequired"));
		integerItem.setHintStyle("hint");
		integerItem.setBrowserSpellCheck(false);
		IntegerItem.setDefaultProperties(integerItem);

		ColorPickerItem colorPickerItem = new ColorPickerItem();
		colorPickerItem.setWidth(200);
		colorPickerItem.setRequiredMessage(I18N.message("fieldrequired"));
		colorPickerItem.setHintStyle("hint");
		colorPickerItem.setBrowserSpellCheck(false);
		ColorPickerItem.setDefaultProperties(colorPickerItem);

		LinkItem linkItem = new LinkItem();
		linkItem.setRequiredMessage(I18N.message("fieldrequired"));
		linkItem.setHintStyle("hint");
		linkItem.setBrowserSpellCheck(false);
		LinkItem.setDefaultProperties(linkItem);

		TextAreaItem textAreaItem = new TextAreaItem();
		textAreaItem.setRequiredMessage(I18N.message("fieldrequired"));
		textAreaItem.setHintStyle("hint");
		textAreaItem.setBrowserSpellCheck(false);
		TextAreaItem.setDefaultProperties(textAreaItem);

		TimeItem timeItem = new TimeItem();
		timeItem.setHintStyle("hint");
		timeItem.setWidth(60);
		timeItem.setBrowserSpellCheck(false);
		TimeItem.setDefaultProperties(timeItem);

		FloatItem floatItem = new FloatItem();
		floatItem.setHintStyle("hint");
		floatItem.setBrowserSpellCheck(false);
		FloatItem.setDefaultProperties(floatItem);

		ColorPickerItem colorItemPicker = new ColorPickerItem();
		colorItemPicker.setHintStyle("hint");
		colorItemPicker.setWidth(115);
		colorItemPicker.setBrowserSpellCheck(false);
		ColorPickerItem.setDefaultProperties(colorItemPicker);

		RichTextItem richTextItem = new RichTextItem();
		richTextItem.setBrowserSpellCheck(false);
		RichTextItem.setDefaultProperties(richTextItem);
	}

	private ItemFactory() {
	}

	public static ColorPickerItem newColorItemPicker(String name, String title, String value, boolean clearOption,
			ChangedHandler changedHandler) {
		ColorPickerItem item = new ColorPickerItem(filterItemName(name));

		if (title != null)
			item.setTitle(I18N.message(title));
		else
			item.setShowTitle(false);

		if (value != null)
			item.setValue(value);

		if (clearOption) {
			PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					item.setValue((String) null);
					if (changedHandler != null)
						changedHandler.onChanged(null);
				}
			});
			item.setIcons(clear);
			item.setIconVAlign(VerticalAlignment.CENTER);
		}

		if (changedHandler != null)
			item.addChangedHandler(changedHandler);
		return item;
	}

	/**
	 * Creates a new DateItem.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (optional)
	 * 
	 * @return the new item
	 */
	public static DateItem newDateItem(String name, String title) {
		DateItem date = new DateItem(filterItemName(name));
		if (title != null)
			date.setTitle(I18N.message(title));
		else
			date.setShowTitle(false);
		return date;
	}

	/**
	 * Creates a new DateItem for the Extended AttributesDS.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title
	 * 
	 * @return the new item
	 */
	public static DateItem newDateItemForAttribute(String name, String title) {
		// We cannot use spaces in items name
		String itemName = "_" + name.replace(" ", Constants.BLANK_PLACEHOLDER);
		final DateItem date = new DateItem(itemName);
		date.setTitle(title);
		return date;
	}

	public static MiniDateRangeItem newMiniDateRangeItem(String name, String title) {
		final MiniDateRangeItem date = new MiniDateRangeItem(filterItemName(name));
		if (title != null)
			date.setTitle(I18N.message(title));
		else
			date.setShowTitle(false);
		return date;
	}

	public static MultiComboBoxItem newMultipleUsersSelector(String name, String title, boolean skipDisabled,
			long[] selection) {
		String[] ids = null;

		if (selection != null) {
			ids = new String[selection.length];
			for (int i = 0; i < ids.length; i++)
				ids[i] = Long.toString(selection[i]);
		}

		MultiComboBoxItem item = ItemFactory.newMultiComboBoxItem(name, title, new UsersDS(null, false, skipDisabled),
				ids);
		item.setValueField("id");
		item.setDisplayField("username");

		ComboBoxItem combo = new ComboBoxItem();
		combo.setHint(I18N.message("enterusers"));

		item.setAutoChildProperties("comboBox", combo);
		return item;
	}

	public static TextItem newFolderSelectorForAttribute(String name, String title, boolean allowsNull,
			List<FormItemIcon> additionalIcons) {
		final TextItem item = new FolderSelector("_" + name.replace(" ", Constants.BLANK_PLACEHOLDER), !allowsNull,
				additionalIcons);
		if (title != null)
			item.setTitle(I18N.message(title));
		return item;
	}

	public static SelectItem newUserSelectorForAttribute(String name, String title, String groupIdOrName,
			List<FormItemIcon> additionalIcons) {
		final SelectItem item = new UserSelector("_" + name.replace(" ", Constants.BLANK_PLACEHOLDER), title,
				groupIdOrName, false, true, additionalIcons);
		return item;
	}

	public static SelectItem newRecipientTypeSelector(String name) {
		SelectItem selector = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("to", I18N.message("to").toUpperCase());
		opts.put("cc", I18N.message("cc").toUpperCase());
		opts.put("bcc", I18N.message("bcc").toUpperCase());
		selector.setValueMap(opts);
		selector.setName(filterItemName(name));
		selector.setShowTitle(false);
		selector.setValue("to");
		selector.setRequired(true);
		return selector;
	}

	public static SelectItem newUserTypeSelector(String name, int userType) {
		SelectItem selector = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("" + GUIUser.TYPE_DEFAULT, I18N.message("default"));
		opts.put("" + GUIUser.TYPE_READONLY, I18N.message("readonly"));
		selector.setValueMap(opts);
		selector.setName(filterItemName(name));
		selector.setTitle(I18N.message("usertype"));
		selector.setValue("" + userType);
		selector.setRequired(true);
		selector.setWidth(120);
		return selector;
	}

	public static SelectItem newDashletTypeSelector(String value) {
		SelectItem selector = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("document", I18N.message("dashlet.type.document"));
		opts.put("docevent", I18N.message("dashlet.type.docevent"));
		opts.put("note", I18N.message("dashlet.type.note"));
		opts.put("content", I18N.message("dashlet.type.content"));
		selector.setValueMap(opts);
		selector.setName("type");
		selector.setTitle(I18N.message("type"));
		selector.setValue(value);
		selector.setRequired(true);
		selector.setWidth(120);
		return selector;
	}

	public static SelectItem newDensitySelector() {
		SelectItem densitySelector = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("dense", I18N.message("dense"));
		opts.put("compact", I18N.message("compact"));
		opts.put("medium", I18N.message("mmedium"));
		opts.put("expanded", I18N.message("expanded"));
		opts.put("spacious", I18N.message("spacious"));

		densitySelector.setValueMap(opts);
		densitySelector.setName("density");
		densitySelector.setTitle(I18N.message("density"));
		densitySelector.setWidth(100);

		densitySelector.setValue(Session.get().getConfig("gui.density"));

		return densitySelector;
	}

	public static SelectItem newSkinSelector() {
		SelectItem skinSelector = new SelectItem();
		skinSelector.setWidth(120);
		skinSelector.setName("skin");
		skinSelector.setTitle(I18N.message("skin"));
		skinSelector.setValueField("name");
		skinSelector.setDisplayField("label");
		skinSelector.setOptionDataSource(new SkinsDS());

		if (Session.get() != null && Session.get().getInfo().getBranding() != null)
			skinSelector.setValue(Session.get().getInfo().getBranding().getSkin());

		return skinSelector;
	}

	public static SelectItem newDateOperator(String name, String title) {
		SelectItem dateOperator = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("before", I18N.message("before"));
		opts.put("after", I18N.message("after"));
		dateOperator.setValueMap(opts);
		dateOperator.setName(filterItemName(name));
		if (title != null)
			dateOperator.setTitle(I18N.message(title));
		else
			dateOperator.setShowTitle(false);
		dateOperator.setWidth(100);
		return dateOperator;
	}

	public static SelectItem newSizeOperator(String name, String title) {
		SelectItem item = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("lessthan", I18N.message("lessthan"));
		opts.put("greaterthan", I18N.message("greaterthan"));
		item.setValueMap(opts);
		item.setName(filterItemName(name));
		if (title != null)
			item.setTitle(I18N.message(title));
		else
			item.setShowTitle(false);
		item.setWidth(100);
		return item;
	}

	public static SelectItem newBarcodeGenerationFormatSelector(String name, String title, String value) {
		SelectItem item = new SelectItem(name, I18N.message(title));
		item.setWrapTitle(false);
		item.setWidth(110);

		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("CODE_128", "CODE_128");
		opts.put("CODE_39", "CODE_39");
		opts.put("EAN_13", "EAN_13");
		opts.put("EAN_8", "EAN_8");
		opts.put("ITF", "ITF");
		opts.put("UPC_A", "UPC_A");
		opts.put("QR_CODE", "QR_CODE");

		item.setValueMap(opts);

		if (value != null)
			item.setValue(value);

		return item;
	}

	public static SelectItem newLanguageSelector(String name, boolean withEmpty, boolean gui) {
		SelectItem item = new SelectItem();
		if (gui)
			item.setValueMap(I18N.getSupportedGuiLanguages(withEmpty));
		else
			item.setValueMap(I18N.getSupportedLanguages(withEmpty));
		item.setName(filterItemName(name));
		item.setTitle(I18N.message("language"));
		item.setWrapTitle(false);
		item.setWidth(120);
		return item;
	}

	public static ComboBoxItem newTimeZoneSelector(String name, String title, String value) {
		ComboBoxItem item = new ComboBoxItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		item.setWrapTitle(false);
		item.setDisplayField("id");
		item.setValueField("id");

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth("*");

		item.setPickListFields(id);
		item.setOptionDataSource(new TimeZonesDS());

		if (value != null)
			item.setValue(value);
		return item;
	}

	public static SelectItem newCharsetSelector(String name) {
		SelectItem item = new SelectItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message("charset"));
		item.setWrapTitle(false);
		item.setDefaultValue("UTF-8");
		item.setDisplayField("name");
		item.setValueField("code");

		ListGridField code = new ListGridField("code", I18N.message("code"));
		code.setWidth(90);
		code.setHidden(true);
		code.setShowTitle(false);

		ListGridField nameField = new ListGridField("name", I18N.message("name"));
		nameField.setWidth("*");
		nameField.setShowTitle(false);

		item.setPickListFields(code, nameField);
		item.setOptionDataSource(new CharsetsDS());

		return item;
	}

	public static SelectItem newStorageSelector(String name, Integer value) {
		SelectItem item = new SelectItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message("storage"));
		item.setWrapTitle(false);
		item.setDisplayField("name");
		item.setValueField("id");
		item.setWidth(150);

		ListGridField nameField = new ListGridField("name", I18N.message("name"));
		nameField.setWidth("*");
		nameField.setShowTitle(false);

		item.setPickListFields(nameField);
		item.setOptionDataSource(new StoragesDS(true, false));

		if (value != null)
			item.setValue(value.toString());

		return item;
	}

	public static SelectItem newStorageTypeSelector() {
		SelectItem item = new SelectItem("type", I18N.message("type"));
		item.setWidth(140);
		item.setWrapTitle(false);
		item.setDisplayField("name");
		item.setValueField("id");
		item.setOptionDataSource(new StoragesTypesDS());
		return item;
	}

	public static SelectItem newAnnotationShapeSelector(String value) {
		SelectItem selector = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("square", I18N.message("square"));
		opts.put("circle", I18N.message("circle"));
		opts.put("line", I18N.message("line"));
		opts.put("arrow", I18N.message("arrow"));
		opts.put("thickarrow", I18N.message("thickarrow"));
		selector.setValueMap(opts);
		selector.setName("shape");
		selector.setTitle(I18N.message("shape"));
		selector.setValue(value);
		selector.setWidth(100);
		return selector;
	}

	public static SelectItem newComparatorSelector() {
		return newComparatorSelector("-");
	}

	public static SelectItem newComparatorSelector(String inExt) {
		SelectItem item = new SelectItem("comparator", I18N.message("comparator"));
		item.setWidth(150);
		item.setWrapTitle(false);
		item.setMultiple(false);
		item.setDisplayField("label");
		item.setValueField("comparator");
		item.setOptionDataSource(new ComparatorsDS(inExt));

		item.setValueFormatter(new FormItemValueFormatter() {

			@Override
			public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
				ListGridRecord r = item.getSelectedRecord();
				String label = r.getAttribute("label");
				if (label == null || "".equals(label))
					return null;

				if (r.getAttribute("eenabled") != null && !r.getAttributeAsBoolean("eenabled"))
					label = "<span style='color:red;'>" + label + "</span>";
				return label;
			}
		});

		ListGridField f = new ListGridField("label");
		f.setCellFormatter(new CellFormatter() {
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				String label = record.getAttribute("label");
				if (label == null || "".equals(label))
					return null;

				if (record.getAttribute("eenabled") != null && !record.getAttributeAsBoolean("eenabled"))
					label = "<span style='color:red;'>" + label + "</span>";
				return label;
			}
		});
		item.setPickListFields(f);

		return item;
	}

	public static SelectItem newFormatConverterSelector() {
		return newFormatConverterSelector("-", "-");
	}

	public static SelectItem newFormatConverterSelector(String inExt, String outExt) {
		SelectItem item = new SelectItem("converter", I18N.message("converter"));
		item.setWidth(150);
		item.setWrapTitle(false);
		item.setMultiple(false);
		item.setDisplayField("label");
		item.setValueField("converter");
		item.setOptionDataSource(new FormatConvertersDS(inExt, outExt));

		item.setValueFormatter(new FormItemValueFormatter() {

			@Override
			public String formatValue(Object value, Record record, DynamicForm form, FormItem item) {
				ListGridRecord r = item.getSelectedRecord();
				String label = r.getAttribute("label");
				if (label == null || "".equals(label))
					return null;

				if (r.getAttribute("eenabled") != null && !r.getAttributeAsBoolean("eenabled"))
					label = "<span style='color:red;'>" + label + "</span>";
				return label;
			}
		});

		ListGridField f = new ListGridField("label");
		f.setCellFormatter(new CellFormatter() {
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				String label = record.getAttribute("label");
				if (label == null || "".equals(label))
					return null;

				if (record.getAttribute("eenabled") != null && !record.getAttributeAsBoolean("eenabled"))
					label = "<span style='color:red;'>" + label + "</span>";
				return label;
			}
		});
		item.setPickListFields(f);

		return item;
	}

	public static TextItem newEmailItem(String name, String title, boolean multiple) {
		TextItem item = new TextItem();
		item.setName(filterItemName(name));
		if (title != null)
			item.setTitle(I18N.message(title));
		else
			item.setShowTitle(false);
		if (multiple)
			item.setValidators(new EmailsValidator());
		else
			item.setValidators(new EmailValidator());
		item.setWrapTitle(false);
		item.setWidth(200);

		return item;
	}

	public static ComboBoxItem newEmailComboSelector(String name, String title) {
		final ComboBoxItem selector = new ComboBoxItem(filterItemName(name));
		selector.setTitle(I18N.message(title));
		selector.setWrapTitle(false);
		selector.setValueField("email");
		selector.setDisplayField("email");
		selector.setPickListWidth(350);
		selector.setFetchDelay(2000);
		selector.setHideEmptyPickList(true);
		ListGridField email = new ListGridField("email", I18N.message("email"));
		email.setWidth("*");
		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"));
		firstName.setWidth(90);
		ListGridField lastName = new ListGridField("lastName", I18N.message("lastname"));
		lastName.setWidth(90);
		selector.setPickListFields(email, firstName, lastName);
		selector.setOptionDataSource(new ContactsDS());
		return selector;
	}

	public static SelectItem newEmailSelector(String name, String title) {
		final SelectItem selector = new SelectItem(filterItemName(name));
		selector.setTitle(I18N.message(title));
		selector.setWrapTitle(false);
		selector.setValueField("email");
		selector.setDisplayField("email");
		selector.setPickListWidth(350);
		selector.setMultiple(true);
		selector.setHideEmptyPickList(true);
		ListGridField email = new ListGridField("email", I18N.message("email"));
		email.setWidth("*");
		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"));
		firstName.setWidth(90);
		ListGridField lastName = new ListGridField("lastName", I18N.message("lastname"));
		lastName.setWidth(90);
		selector.setPickListFields(email, firstName, lastName);
		selector.setOptionDataSource(new ContactsDS());
		return selector;
	}

	public static SelectItem newEmailFromSelector(String name, String title) {
		final SelectItem selector = new SelectItem(filterItemName(name));
		if (title != null)
			selector.setTitle(I18N.message(title));
		else
			selector.setTitle(I18N.message("from"));
		selector.setWidth(300);
		selector.setWrapTitle(false);
		selector.setValueField("email");
		selector.setDisplayField("email");
		selector.setHideEmptyPickList(true);

		if (Session.get().getUser().getEmail2() != null && !Session.get().getUser().getEmail2().isEmpty())
			selector.setValueMap(Session.get().getUser().getEmail(), Session.get().getUser().getEmail2());
		else
			selector.setValueMap(Session.get().getUser().getEmail());

		selector.setValue(Session.get().getUser().getEmail());

		return selector;
	}

	public static SelectItem newGroupSelector(String name, String title) {
		SelectItem group = new SelectItem(filterItemName(name));
		group.setTitle(I18N.message(title));
		group.setWrapTitle(false);
		group.setValueField("id");
		group.setDisplayField("name");
		group.setPickListWidth(300);
		ListGridField n = new ListGridField("name", I18N.message("name"));
		ListGridField description = new ListGridField("description", I18N.message("description"));
		group.setPickListFields(n, description);
		group.setOptionDataSource(new GroupsDS());
		return group;
	}

	public static SelectItem newUserSelector(String name, String title, String groupIdOrName, boolean required,
			boolean skipDisabled) {
		final SelectItem item = new UserSelector(name, title, groupIdOrName, !required, skipDisabled, null);
		return item;
	}

	public static SelectItem newTenantSelector() {
		SelectItem tenant = new SelectItem("tenant");
		tenant.setTitle(I18N.message("tenant"));
		tenant.setWrapTitle(false);
		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setHidden(true);
		ListGridField _name = new ListGridField("name", I18N.message("name"));
		_name.setAutoFitWidth(true);
		ListGridField displayName = new ListGridField("displayName", I18N.message("displayname"));
		displayName.setWidth("*");
		tenant.setValueField("id");
		tenant.setDisplayField("displayName");
		tenant.setPickListWidth(300);
		tenant.setWidth(150);
		tenant.setPickListFields(id, _name, displayName);
		tenant.setOptionDataSource(new TenantsDS());
		tenant.setEmptyDisplayValue(" ");
		return tenant;
	}

	public static SelectItem newAutomationRoutineSelector(String name, Long value, final ChangedHandler handler,
			boolean showEmpty) {
		final SelectItem select = newSelectItem(filterItemName(name), "routine");
		select.setValueField("id");
		select.setDisplayField("name");
		select.setEmptyDisplayValue(I18N.message("customcode"));

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setHidden(true);
		ListGridField _name = new ListGridField("name", I18N.message("name"));
		ListGridField description = new ListGridField("description", I18N.message("description"));
		select.setPickListFields(id, _name, description);
		select.setPickListWidth(250);
		select.setOptionDataSource(new AutomationRoutinesDS(showEmpty));
		select.setValue(value);

		ChangedHandler setNullOnEmpty = new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if (event != null && event.getValue().equals("")) {
					select.setValue((String) null);
					select.clearValue();
				}
			}
		};
		select.addChangedHandler(setNullOnEmpty);
		if (handler != null)
			select.addChangedHandler(handler);

		return select;
	}

	public static RadioGroupItem newRadioGroup(String name, String title) {
		RadioGroupItem radioGroupItem = new RadioGroupItem();
		radioGroupItem.setName(filterItemName(name));
		radioGroupItem.setVertical(false);
		radioGroupItem.setTitle(I18N.message(title));
		radioGroupItem.setWidth(80);
		return radioGroupItem;
	}

	public static RadioGroupItem newBooleanSelector(String name, String title) {
		RadioGroupItem radioGroupItem = newRadioGroup(name, title);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("yes", I18N.message("yes"));
		map.put("no", I18N.message("no"));
		radioGroupItem.setValueMap(map);
		return radioGroupItem;
	}

	public static CheckboxItem newCheckbox(String name, String title) {
		CheckboxItem item = new CheckboxItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		return item;
	}

	public static MultiComboBoxItem newMultiComboBoxItem(String name, String title, DataSource options,
			Object[] values) {
		MultiComboBoxItem item = new MultiComboBoxItem(name, I18N.message(title));
		item.setLayoutStyle(MultiComboBoxLayoutStyle.FLOW);
		item.setWidth(200);
		item.setMultiple(true);
		if (options != null) {
			item.setOptionDataSource(options);
			item.setAutoFetchData(true);
		}

		IButton closeButton = new IButton();
		closeButton.setIcon("[SKIN]/headerIcons/close.gif");
		closeButton.setIconOrientation("body");
		item.setButtonProperties(closeButton);

		if (values != null) {
			item.setValue(values);
		}

		return item;
	}

	public static MultiComboBoxItem newTagsComboBoxItem(String name, String title, TagsDS options, Object[] tags) {
		MultiComboBoxItem item = newMultiComboBoxItem(name, title, options, tags);
		item.setPrompt(I18N.message("typeatag"));
		item.setValueField("word");
		item.setDisplayField("word");
		return item;
	}

	public static MultiComboBoxItem newBarcodeFormatsComboBoxItem(String name, String title, String values) {
		return newBarcodeFormatsComboBoxItem(name, title, values != null ? values.split(",") : null);
	}

	public static MultiComboBoxItem newBarcodeFormatsComboBoxItem(String name, String title, String[] values) {
		MultiComboBoxItem item = new MultiComboBoxItem(name, I18N.message(title));
		item.setLayoutStyle(MultiComboBoxLayoutStyle.FLOW);
		item.setWidth(200);
		item.setMultiple(true);

		IButton closeButton = new IButton();
		closeButton.setIcon("[SKIN]/headerIcons/close.gif");
		closeButton.setIconOrientation("body");
		item.setButtonProperties(closeButton);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("CODE_39", "CODE_39");
		map.put("CODE_128", "CODE_128");
		map.put("EAN_8", "EAN_8");
		map.put("EAN_13", "EAN_13");
		map.put("QR_CODE", "QR_CODE");
		map.put("AZTEC", "AZTEC");
		map.put("CODE_93", "CODE_93");
		map.put("RSS_14", "RSS_14");
		map.put("RSS_EXPANDED", "RSS_EXPANDED");
		map.put("UPC_A", "UPC_A");
		map.put("UPC_E", "UPC_E");
		map.put("DATA_MATRIX", "DATA_MATRIX");
		map.put("ITF", "ITF");
		map.put("PDF_417", "PDF_417");
		map.put("CODABAR", "CODABAR");
		map.put("MAXICODE", "MAXICODE");
		item.setValueMap(map);

		if (values != null)
			item.setValue(values);

		return item;
	}

	public static SelectItem newTagsMultiplePickList(String name, String title, TagsDS options, Object[] tags) {
		final SelectItem item = newSelectItem(name, title);
		item.setMultiple(true);
		item.setMultipleAppearance(MultipleAppearance.PICKLIST);
		item.setValueField("word");
		item.setDisplayField("word");
		item.setOptionDataSource(options);
		if (tags != null)
			item.setValue(tags);
		return item;
	}

	public static SelectItem newMultipleSelector(String name, String title) {
		SelectItem selectItemMultipleGrid = new SelectItem();
		selectItemMultipleGrid.setName(filterItemName(name));
		selectItemMultipleGrid.setTitle(I18N.message(title));
		selectItemMultipleGrid.setMultiple(true);
		selectItemMultipleGrid.setValueMap("");
		return selectItemMultipleGrid;
	}

	public static SelectItem newPrioritySelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), title);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("low"));
		map.put("1", I18N.message("medium"));
		map.put("2", I18N.message("high"));
		select.setValueMap(map);
		select.setValue("0");
		return select;
	}

	public static SelectItem newWelcomeScreenSelector(Integer value) {
		SelectItem select = new SelectItem("welcomescreen", I18N.message("welcomescreen"));
		select.setWidth(150);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("1500", I18N.message("documents"));
		map.put("1510", I18N.message("search"));
		map.put("1520", I18N.message("dashboard"));
		select.setValueMap(map);
		if (value != null)
			select.setValue(value.toString());
		else
			select.setValue("1500");
		return select;
	}

	public static SelectItem newDashletSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), title);
		select.setAllowEmptyValue(false);
		select.setOptionDataSource(new DashletsDS());
		select.setPickListWidth(200);
		select.setDisplayField("title");
		select.setValueField("name");

		return select;
	}

	public static SelectItem newOCRStatusSelector(Integer value) {
		SelectItem select = new SelectItem("ocrstatus", I18N.message("processedbyzonalocr"));
		select.setWidth(150);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("no"));
		map.put("1", I18N.message("yes"));
		map.put("2", I18N.message("skip"));
		select.setValueMap(map);
		if (value != null)
			select.setValue(value.toString());
		else
			select.setValue("2");
		return select;
	}

	public static SelectItem newFolderSecurityOption(String name, String title) {
		SelectItem securityOption = newSelectItem(name, title);
		securityOption.setWidth("*");
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("none", I18N.message("none").toLowerCase());
		opts.put("inherit", I18N.message("inheritparentsec").toLowerCase());
		opts.put("replicate", I18N.message("replicatesourcesec").toLowerCase());
		securityOption.setValueMap(opts);

		String defaultSetting = Session.get().getInfo().getConfig("gui.security.inheritoption.default");
		if (defaultSetting == null || (!"none".equals(defaultSetting) && !"inherit".equals(defaultSetting)
				&& !"replicate".equals(defaultSetting)))
			securityOption.setValue("none");
		else
			securityOption.setValue(defaultSetting);
		return securityOption;
	}

	public static SelectItem newEventsSelector(String name, String title, final ChangedHandler handler, boolean folder,
			boolean workflow, boolean user, boolean importfolder) {
		final SelectItem select = newMultipleSelector(filterItemName(name), title);
		select.setWidth(350);
		select.setHeight(250);
		select.setMultipleAppearance(MultipleAppearance.GRID);
		select.setMultiple(true);
		select.setOptionDataSource(new EventsDS(folder, workflow, user, importfolder));
		select.setValueField("code");
		select.setDisplayField("label");
		if (handler != null)
			select.addChangedHandler(handler);

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				select.clearValue();
				select.setValue((String) null);
				if (handler != null)
					handler.onChanged(null);
			}
		});
		clear.setWidth(12);
		clear.setHeight(12);
		select.setIcons(clear);

		return select;
	}

	public static SelectItem newEventSelector(String name, String title, final ChangedHandler handler, boolean folder,
			boolean workflow, boolean user, boolean importfolder) {
		final SelectItem select = newSelectItem(filterItemName(name), title);
		select.setWidth(350);
		select.setMultiple(false);
		select.setOptionDataSource(new EventsDS(folder, workflow, user, importfolder));
		select.setValueField("code");
		select.setDisplayField("label");
		if (handler != null)
			select.addChangedHandler(handler);

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				select.clearValue();
				select.setValue((String) null);
				if (handler != null)
					handler.onChanged(null);
			}
		});
		clear.setWidth(12);
		clear.setHeight(12);
		select.setIcons(clear);

		return select;
	}

	public static SelectItem newSelectItem(String name, String title) {
		SelectItem select = newMultipleSelector(filterItemName(name),
				title != null ? I18N.message(title) : I18N.message(name));
		select.setMultiple(false);
		select.setWrapTitle(false);
		return select;
	}

	public static SelectItem newConversionFormatItem(String fileName) {
		SelectItem select = ItemFactory.newSelectItem("format", I18N.message("format"));
		select.setMultiple(false);
		select.setWrapTitle(false);
		select.setOptionDataSource(new ConversionFormatsDS(fileName));
		select.setValueField("extension");
		select.setDisplayField("extension");
		return select;
	}

	public static ComboBoxItem newComboBoxItem(String name, String title) {
		ComboBoxItem item = new ComboBoxItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		item.setMultiple(true);
		item.setValueMap("");
		item.setWrapTitle(false);
		return item;
	}

	public static SelectItem newYesNoSelectItem(String name, String title) {
		SelectItem item = newSelectItem(name, title);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("true", I18N.message("yes"));
		map.put("false", I18N.message("no"));
		item.setValueMap(map);
		item.setValue("true");
		item.setWidth(50);
		return item;
	}

	public static SpinnerItem newSpinnerItem(String name, String title, Long value) {
		SpinnerItem spinner = new SpinnerItem(name);
		spinner.setTitle(I18N.message(title));
		spinner.setMin(0);
		spinner.setStep(1);
		spinner.setWidth(60);
		if (value != null)
			spinner.setValue(value.intValue());
		else
			spinner.setValue((Integer) null);
		return spinner;
	}

	public static SpinnerItem newSpinnerItem(String name, String title, Integer value) {
		if (value != null)
			return newSpinnerItem(name, title, value.longValue());
		else
			return newSpinnerItem(name, title, (Long) null);
	}

	public static Img newImgIcon(String name) {
		Img img = newImg(name);
		img.setWidth("16px");
		return img;
	}

	public static Img newImg(String name) {
		Img img = new Img("[SKIN]/" + name);
		return img;
	}

	public static Img newBrandImg(String name, GUIInfo info) {
		Img img = null;

		if (name.equals("logo.png"))
			img = new Img(info.getBranding().getLogoSrc());
		else if (name.equals("logo_head.png"))
			img = new Img(info.getBranding().getLogoHeadSrc());
		else if (name.equals("logo_oem.png"))
			img = new Img(info.getBranding().getLogoOemSrc());
		else if (name.equals("logo_head_oem.png"))
			img = new Img(info.getBranding().getLogoHeadOemSrc());
		else if (name.equals("banner.png"))
			img = new Img(info.getBranding().getBannerSrc());
		else if (name.equals("favicon.png"))
			img = new Img(info.getBranding().getFaviconSrc());
		else if (name.equals("logo_menu.png"))
			img = new Img(info.getBranding().getLogoMenuSrc());
		return img;
	}

	public static FormItemIcon newItemIcon(String image) {
		FormItemIcon icon = new FormItemIcon();
		icon.setSrc(ItemFactory.newImgIcon(image).getSrc());
		return icon;
	}

	public static HeaderIcon newHeaderIcon(String image) {
		HeaderIcon icon = new HeaderIcon(ItemFactory.newImgIcon(image).getSrc());
		return icon;
	}

	public static TextItem newCronExpressionItem(String name, String title, String value, ChangedHandler handler) {
		TextItem cron = ItemFactory.newTextItem(name != null ? name : "cron",
				title != null ? I18N.message(title) : I18N.message("cronexpression"), value);
		if (handler != null)
			cron.addChangedHandler(handler);
		cron.setWidth(230);

		FormItemIcon composer = new FormItemIcon();
		composer.setPrompt(I18N.message("opencronexpressioncomposer"));
		composer.setSrc("[SKIN]/DynamicForm/date_control.png");
		composer.setWidth(16);
		composer.setHeight(16);
		composer.addFormItemClickHandler(new FormItemClickHandler() {
			public void onFormItemClick(final FormItemIconClickEvent event) {
				CronExpressionComposer comp = new CronExpressionComposer(cron, handler);
				comp.show();
				event.cancel();
			}
		});

		FormItemIcon validate = new FormItemIcon();
		validate.setPrompt(I18N.message("validate"));
		validate.setSrc("[SKIN]/actions/approve.png");
		validate.setWidth(16);
		validate.setHeight(16);
		validate.addFormItemClickHandler(new FormItemClickHandler() {
			public void onFormItemClick(final FormItemIconClickEvent event) {
				InfoService.Instance.get().getCronDescription(cron.getValueAsString(), I18N.getLocale(),
						new AsyncCallback<String>() {

							@Override
							public void onFailure(Throwable caught) {
								cron.setErrors(caught.getMessage());
								cron.getForm().getValuesManager().showErrors();
							}

							@Override
							public void onSuccess(String description) {
								GuiLog.info(I18N.message("conexprcorrect", description));
								cron.clearErrors();
								cron.getForm().getValuesManager().showErrors();
							}
						});
			}
		});

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				cron.setValue((String) null);
				cron.clearErrors();
				if (handler != null)
					handler.onChanged(null);
			}
		});
		clear.setWidth(16);
		clear.setHeight(16);
		cron.setIcons(composer, validate, clear);

		return cron;
	}

	/**
	 * Creates a new TextItem.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the text item
	 */
	public static TextItem newTextItem(String name, String title, String value) {
		TextItem item = new TextItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		else
			item.setValue("");
		item.setWrapTitle(false);
		item.setRequiredMessage(I18N.message("fieldrequired"));
		return item;
	}

	/**
	 * Creates a new TextItem.
	 * 
	 * @param name The item name (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the text item
	 */
	public static TextItem newTextItem(String name, String value) {
		return newTextItem(name, name, value);
	}

	public static TextItem newTextItemPreventAutocomplete(String name, String title, String value) {
		TextItem item = newTextItem(name, title, value);
		item.setAutoCompleteKeywords("nope");
		return item;
	}

	public static TextItem newTextItemForAutomation(String name, String title, String value, ChangedHandler handler) {
		TextItem item = newTextItem(name, title, value);
		appendAutomationEditorIcon(item, handler, false);
		if (handler != null)
			item.addChangedHandler(handler);
		return item;
	}

	public static RichTextItem newRichTextItemForAutomation(String name, String title, String value,
			ChangedHandler handler) {
		RichTextItem item = new RichTextItem(name, I18N.message(title));
		item.setOverflow(Overflow.HIDDEN);

		appendAutomationEditorIcon(item, handler, true);
		item.setValue(value);
		if (handler != null)
			item.addChangedHandler(handler);
		return item;
	}

	public static RowSpacerItem newRowSpacer() {
		RowSpacerItem item = new RowSpacerItem();
		item.setCellStyle("row");
		item.setHeight(5);
		return item;
	}

	/**
	 * Creates a new TextItem for an extended attribute
	 * 
	 * @param att the attribute
	 * 
	 * @return the new item
	 */
	public static FormItem newStringItemForAttribute(GUIAttribute att) {
		// We cannot use spaces in items name
		String itemName = "_" + att.getName().replace(" ", Constants.BLANK_PLACEHOLDER);
		final String initialValue = att.getStringValue();

		FormItem item = null;
		if (att.getEditor() == GUIAttribute.EDITOR_TEXTAREA) {
			item = newTextAreaItem(itemName, itemName, initialValue);
			item.setWidth(Session.get().getConfigAsInt("gui.textarea.w"));
			item.setHeight(Session.get().getConfigAsInt("gui.textarea.h"));
		} else {
			item = newTextItem(itemName, itemName, initialValue);

			if (!InputValues.getInputs(itemName).isEmpty()) {
				item = formItemWithSuggestions(item);
			}

			if (att.getEditor() == GUIAttribute.EDITOR_LISTBOX && att.getSetId() != null) {
				item = buildComboBoxItemForStringAttribute(att, itemName, initialValue);
			}

			item.setTooltip(item.getValue() != null ? item.getValue().toString() : "");
			item.setWidth(Session.get().getConfigAsInt("gui.textbox.w"));
		}

		item.setName(itemName);
		if (att.getLabel() != null)
			item.setTitle(att.getLabel());
		else
			item.setTitle(att.getName());
		item.setWrapTitle(false);

		return item;
	}

	private static FormItem buildComboBoxItemForStringAttribute(GUIAttribute att, String itemName,
			final String initialValue) {
		FormItem item;
		item = new ComboBoxItem(itemName);

		final AttributeOptionsDS options = new AttributeOptionsDS(att.getSetId(),
				att.getParent() != null ? att.getParent() : att.getName(), null, !att.isMandatory());
		options.setCacheAllData(true);
		item.setOptionDataSource(options);
		item.setValueField("value");
		item.setDisplayField("value");
		item.setFetchMissingValues(true);
		item.setAlwaysFetchMissingValues(true);
		item.setValue(initialValue);

		// When the user clicks on the item, preemptively load all the
		// options to correctly do text completion
		item.addEditorEnterHandler(new EditorEnterHandler() {

			@Override
			public void onEditorEnter(EditorEnterEvent event) {
				options.fetchData(new Criteria());
			}
		});

		// When the user ends the editing, restore the initial value if
		// he does not select an existing option
		item.addEditorExitHandler(new EditorExitHandler() {

			@Override
			public void onEditorExit(EditorExitEvent event) {
				String val = event.getValue() != null ? event.getValue().toString() : null;
				Record[] records = options.getCacheData();
				boolean found = false;
				if (records != null)
					for (Record record : records) {
						if (record.getAttributeAsString("value").equals(val)) {
							found = true;
							break;
						}
					}
				if (!found)
					event.getItem().setValue(initialValue);
			}
		});

		if (!att.isMandatory())
			((ComboBoxItem) item).setAllowEmptyValue(true);
		return item;
	}

	public static PasswordItem newPasswordItem(String name, String title, String value) {
		PasswordItem password = new PasswordItem();
		password.setTitle(I18N.message(title));
		password.setName(filterItemName(name));
		if (value != null)
			password.setValue(value);
		return password;
	}

	public static PasswordItem newPasswordItemPreventAutocomplete(String name, String title, String value) {
		PasswordItem password = newPasswordItem(name, title, value);
		password.setAutoCompleteKeywords("new-password");
		return password;
	}

	public static PasswordItem newPasswordItemPreventAutocomplete(String name, String title, String value,
			boolean withGeneratorTool) {
		PasswordItem password = newPasswordItemPreventAutocomplete(name, title, value);
		FormItemIcon generator = new FormItemIcon();
		generator.setName("generator");
		generator.setWidth(16);
		generator.setHeight(16);

		if (withGeneratorTool) {
			generator.setSrc("[SKIN]/key.png");
			generator.setPrompt(I18N.message("passwordgenerator"));
			generator.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					PasswordGenerator generator = new PasswordGenerator();
					generator.show();
				}
			});
		}

		password.setIcons(generator);
		password.setIconVAlign(VerticalAlignment.CENTER);

		return password;
	}

	/**
	 * Provides a control that does not display the password input directly, it
	 * uses a second item to store the password in an hidden attribute of the
	 * form.
	 * 
	 * @param name Name of the field
	 * @param title The title
	 * @param value Current password value
	 * @param hiddenPasswordItem The item that stores the real password
	 * @param changedHandler optional handler to invoke when the password has
	 *        been changed
	 * 
	 * @return the opaque item
	 */
	public static StaticTextItem newSafePasswordItem(String name, String title, String value,
			FormItem hiddenPasswordItem, ChangedHandler changedHandler) {
		StaticTextItem item = newStaticTextItem(name, title,
				value == null || value.toString().isEmpty() ? "" : "*****");

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				item.setValue((String) null);
				hiddenPasswordItem.setValue((String) null);
				if (changedHandler != null)
					changedHandler.onChanged(null);
			}
		});

		FormItemIcon edit = new FormItemIcon();
		edit.setName("edit");
		edit.setWidth(16);
		edit.setHeight(16);
		edit.setSrc("[SKIN]/edit.png");
		edit.setPrompt(I18N.message("changepassword"));
		edit.addFormItemClickHandler(new FormItemClickHandler() {

			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				PasswordItem password = newPasswordItem("psw", title, null);
				password.setAutoCompleteKeywords("new-password");
				password.setShowTitle(false);
				LD.askForValue(title, "password", null, password, new ValueCallback() {

					@Override
					public void execute(String value) {
						item.setValue("*****");
						hiddenPasswordItem.setValue(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					}
				});
			}
		});

		item.setIcons(edit, clear);
		item.setIconVAlign(VerticalAlignment.CENTER);

		return item;
	}

	/**
	 * Creates a new TextItem that validates a simple text
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static TextItem newSimpleTextItem(String name, String title, String value) {
		TextItem item = newTextItem(filterItemName(name), I18N.message(title), value);
		item.setValidators(new SimpleTextValidator());
		return item;
	}

	public static TextItem newSimpleTextItemPreventAutocomplete(String name, String title, String value) {
		TextItem item = newSimpleTextItem(name, title, value);
		item.setAutoCompleteKeywords("nope");
		return item;
	}

	/**
	 * Creates a new StaticTextItem
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static StaticTextItem newStaticTextItem(String name, String title, String value) {
		StaticTextItem item = new StaticTextItem();
		if (name.trim().isEmpty())
			item.setShouldSaveValue(false);
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		else
			item.setValue("");
		item.setWrapTitle(false);
		return item;
	}

	/**
	 * Creates a new IntegerItem
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static IntegerItem newLongItem(String name, String title, Long value) {
		IntegerItem item = new IntegerItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		IsIntegerValidator iv = new IsIntegerValidator();
		iv.setErrorMessage(I18N.message("wholenumber"));
		item.setValidators(iv);
		return item;
	}

	/**
	 * Creates a new IntegerItem.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static IntegerItem newIntegerItem(String name, String title, Integer value) {
		IntegerItem item = new IntegerItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		IsIntegerValidator iv = new IsIntegerValidator();
		iv.setErrorMessage(I18N.message("wholenumber"));
		item.setValidators(iv);
		return item;
	}

	/**
	 * Creates a new IntegerItem for the Extended AttributesDS.
	 * 
	 * @param name The item name (mandatory)
	 * @param label The item label
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static FormItem newIntegerItemForAttribute(String name, String label, Integer value) {
		// We cannot use spaces in items name
		String itemName = "_" + name.replace(" ", Constants.BLANK_PLACEHOLDER);
		FormItem item = newIntegerItem(itemName, label, value);
		if (!InputValues.getInputs(item.getName()).isEmpty()) {
			item = formItemWithSuggestions(item);
			IsIntegerValidator iv = new IsIntegerValidator();
			iv.setErrorMessage(I18N.message("wholenumber"));
			item.setValidators(iv);
		}
		return item;
	}

	/**
	 * Simple boolean selector for Extended Attribute
	 * 
	 * @param name name of the item
	 * @param title title of the item(optional)
	 * @param allowEmpty flag to indicate id the item must accept empty value
	 * 
	 * @return the new item
	 */
	public static SelectItem newBooleanSelectorForAttribute(String name, String title, boolean allowEmpty) {
		String itemName = "_" + name.replace(" ", Constants.BLANK_PLACEHOLDER);
		SelectItem select = new SelectItem();
		select.setName(itemName);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		if (allowEmpty)
			map.put("", " ");

		map.put("1", I18N.message("yes"));
		map.put("0", I18N.message("no"));
		select.setValueMap(map);
		select.setTitle(I18N.message(title));
		select.setWidth(80);
		select.setValue("");

		return select;
	}

	/**
	 * Creates a new IntegerItem with a range validator
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * @param min The item minimum value (optional)
	 * @param max The item maximum value (optional)
	 * 
	 * @return the new item
	 */
	public static IntegerItem newValidateIntegerItem(String name, String title, Integer value, Integer min,
			Integer max) {
		IntegerItem item = newIntegerItem(filterItemName(name), I18N.message(title), value);
		IntegerRangeValidator rv = null;
		if (min != null || max != null) {
			rv = new IntegerRangeValidator();
			if (min != null)
				rv.setMin(min);
			if (max != null)
				rv.setMax(max);
		}
		IsIntegerValidator iv = new IsIntegerValidator();
		iv.setErrorMessage(I18N.message("wholenumber"));
		if (rv == null)
			item.setValidators(iv);
		else
			item.setValidators(iv, rv);
		return item;
	}

	/**
	 * Creates a new SpinnerItem(with a range validator)
	 * 
	 * @param name name of the item
	 * @param title title of the item(optional)
	 * @param value default value(optional)
	 * @param min minimum value(optional)
	 * @param max maximum value(optional)
	 * 
	 * @return the new item
	 */
	public static SpinnerItem newSpinnerItem(String name, String title, Integer value, Integer min, Integer max) {
		SpinnerItem spinner = new SpinnerItem(name);
		spinner.setTitle(I18N.message(title));
		spinner.setWrapTitle(false);
		spinner.setDefaultValue(value);
		if (min != null)
			spinner.setMin(min);
		if (max != null)
			spinner.setMax(max);
		spinner.setStep(1);
		spinner.setWriteStackedIcons(true);
		return spinner;
	}

	/**
	 * Creates a new SliderItem
	 * 
	 * @param name name of the item
	 * @param title title of the item(optional)
	 * @param value default value(optional)
	 * @param minValue minimum value
	 * @param maxValue maximum value
	 * 
	 * @return the new item
	 */
	public static SliderItem newSliderItem(String name, String title, Double value, double minValue, double maxValue) {
		SliderItem item = new SliderItem(name, I18N.message(title));
		item.setMinValue(minValue);
		item.setMaxValue(maxValue);
		item.setValue(value);
		item.setWidth(100);
		item.setVertical(false);
		item.setRoundValues(true);
		return item;
	}

	public static LinkItem newLinkItem(String name, String title) {
		LinkItem linkItem = new LinkItem(filterItemName(name));
		if (!title.trim().isEmpty()) {
			linkItem.setTitle(I18N.message(title));
			linkItem.setLinkTitle(I18N.message(title));
		}
		linkItem.setWrapTitle(false);
		return linkItem;
	}

	/**
	 * Creates a new TextAreaItem
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static TextAreaItem newTextAreaItem(String name, String title, String value) {
		TextAreaItem item = new TextAreaItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		item.setHeight("*");
		if (value != null)
			item.setValue(value);
		else
			item.setValue("");
		return item;
	}

	public static TextAreaItem newTextAreaItemForAutomation(String name, String title, String value,
			ChangedHandler handler, boolean withHtmlEditor) {
		TextAreaItem item = newTextAreaItem(name, title, value);
		appendAutomationEditorIcon(item, handler, withHtmlEditor);
		if (handler != null)
			item.addChangedHandler(handler);
		return item;
	}

	private static void appendAutomationEditorIcon(FormItem item, ChangedHandler handler, boolean withHtmlEditor) {
		FormItemIcon editAutomation = new FormItemIcon();
		editAutomation.setName("editautomation");
		editAutomation.setWidth(16);
		editAutomation.setHeight(16);
		editAutomation.setSrc("[SKIN]/java.png");
		editAutomation.setPrompt(I18N.message("openautomationeditor"));
		editAutomation.addFormItemClickHandler(new FormItemClickHandler() {

			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				AutomationItemEditor editor = new AutomationItemEditor(item, handler);
				editor.show();
			}
		});

		if (withHtmlEditor)
			item.setIcons(editAutomation, prepareEditHtmlIcon(item, handler));
		else
			item.setIcons(editAutomation);
		item.setIconVAlign(VerticalAlignment.CENTER);
	}

	private static FormItemIcon prepareEditHtmlIcon(FormItem item, ChangedHandler handler) {
		FormItemIcon editHtml = new FormItemIcon();
		editHtml.setName("edithtml");
		editHtml.setWidth(16);
		editHtml.setHeight(16);
		editHtml.setSrc("[SKIN]/html.png");
		editHtml.setPrompt(I18N.message("openhtmleditor"));
		editHtml.addFormItemClickHandler(new FormItemClickHandler() {

			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				HtmlItemEditor editor = new HtmlItemEditor(item, handler);
				editor.show();
			}
		});
		return editHtml;
	}

	public static TextAreaItem newTextAreaItemForHTML(String name, String title, String value, ChangedHandler handler) {
		TextAreaItem item = newTextAreaItem(name, title, value);
		item.setIcons(prepareEditHtmlIcon(item, handler));
		item.setIconVAlign(VerticalAlignment.CENTER);
		if (handler != null)
			item.addChangedHandler(handler);
		return item;
	}

	public static SelectItem newDueTimeSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));
		select.setWidth(90);
		select.setShowTitle(false);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("minute", I18N.message("minutes"));
		map.put("hour", I18N.message("hours"));
		map.put("day", I18N.message("ddays"));
		select.setValueMap(map);
		select.setValue("minute");
		return select;
	}

	public static TimeItem newTimeItem(String name, String title) {
		TimeItem item = new TimeItem(name, I18N.message(title));
		return item;
	}

	public static TimeItem newTimeItemPicklist(String name, String title) {
		TimeItem item = newTimeItem(name, title);
		item.setUseTextField(false);
		item.setHourItemTitle(I18N.message("hour"));
		item.setMinuteItemTitle(I18N.message("minute"));
		item.setSecondItemTitle(I18N.message("second"));
		item.setShowSecondItem(false);
		return item;
	}

	public static SelectItem newTemplateSelector(boolean withEmpty, Long selectedTemplateId) {
		SelectItem templateItem = new SelectItem("template", I18N.message("template"));
		templateItem.setDisplayField("name");
		templateItem.setValueField("id");
		templateItem.setWidth(150);
		templateItem.setMultiple(false);
		templateItem.setWrapTitle(false);
		templateItem.setMultipleAppearance(MultipleAppearance.PICKLIST);
		templateItem.setOptionDataSource(new TemplatesDS(withEmpty, selectedTemplateId, null));

		if (!Feature.enabled(Feature.TEMPLATE))
			templateItem.setDisabled(true);
		if (selectedTemplateId != null)
			templateItem.setValue(selectedTemplateId.toString());
		return templateItem;
	}

	/**
	 * Creates a select list with the OCR templates
	 * 
	 * @param withEmpty id an empty row must be shown
	 * @param filterTemplateId the document template id to filter the ocr
	 *        templates
	 * @param selectedTemplateId identifier of the template to be selected by
	 *        default
	 * 
	 * @return the item
	 */
	public static SelectItem newOCRTemplateSelector(boolean withEmpty, Long filterTemplateId, Long selectedTemplateId) {
		SelectItem templateItem = new SelectItem("ocrtemplate", I18N.message("ocrtemplate"));
		templateItem.setDisplayField("name");
		templateItem.setValueField("id");
		templateItem.setWidth(150);
		templateItem.setMultiple(false);
		templateItem.setWrapTitle(false);
		templateItem.setMultipleAppearance(MultipleAppearance.PICKLIST);
		templateItem.setOptionDataSource(new OCRTemplatesDS(withEmpty, filterTemplateId));

		if (selectedTemplateId != null)
			templateItem.setValue(selectedTemplateId.toString());
		return templateItem;
	}

	/**
	 * Creates a select list with the barcode templates
	 * 
	 * @param withEmpty id an empty row must be shown
	 * @param filterTemplateId the document template id to filter the ocr
	 *        templates
	 * @param selectedTemplateId identifier of the template to be selected by
	 *        default
	 * 
	 * @return the item
	 */
	public static SelectItem newBarcodeTemplateSelector(boolean withEmpty, Long filterTemplateId,
			Long selectedTemplateId) {
		SelectItem templateItem = new SelectItem("barcodetemplate", I18N.message("barcodetemplate"));
		templateItem.setDisplayField("name");
		templateItem.setValueField("id");
		templateItem.setWidth(150);
		templateItem.setMultiple(false);
		templateItem.setWrapTitle(false);
		templateItem.setMultipleAppearance(MultipleAppearance.PICKLIST);
		templateItem.setOptionDataSource(new BarcodeTemplatesDS(withEmpty, filterTemplateId));

		if (selectedTemplateId != null)
			templateItem.setValue(selectedTemplateId.toString());
		return templateItem;
	}

	public static SelectItem newAttributeSetSelector() {
		final SelectItem selectItem = new SelectItem("attributeset", I18N.message("attributeset"));
		selectItem.setMultiple(false);
		selectItem.setMultipleAppearance(MultipleAppearance.PICKLIST);
		selectItem.setDisplayField("name");
		selectItem.setValueField("id");
		selectItem.setWidth(120);
		selectItem.setOptionDataSource(new AttributeSetsDS(false, GUIAttributeSet.TYPE_DEFAULT));
		selectItem.setWrapTitle(false);

		return selectItem;
	}

	public static SelectItem newAttributesSelector(String context) {
		final SelectItem selectItem = new SelectItem("attributes", I18N.message("attributes"));
		selectItem.setMultiple(true);
		selectItem.setMultipleAppearance(MultipleAppearance.PICKLIST);
		selectItem.setDisplayField("label");
		selectItem.setValueField("name");
		selectItem.setPickListWidth(150);
		selectItem.setOptionDataSource(new AttributesDS(context));
		selectItem.setWrapTitle(false);

		// Make the options ordered by label
		SortSpecifier sp = new SortSpecifier("label", SortDirection.ASCENDING);
		selectItem.setPickListSort(new SortSpecifier[] { sp });

		return selectItem;
	}

	public static SelectItem newAttributesSelector() {
		return newAttributesSelector(null);
	}

	public static SelectItem newFrequencySelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", "");
		map.put("1", I18N.message("daily"));
		map.put("7", I18N.message("weekly"));
		map.put("15", I18N.message("biweekly"));
		map.put("30", I18N.message("monthly"));
		map.put("180", I18N.message("sixmonthly"));
		map.put("365", I18N.message("yearly"));

		select.setValueMap(map);
		select.setWidth(100);
		return select;
	}

	public static SelectItem newCalendarEventStatusSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", "");
		map.put("1", I18N.message("working"));
		map.put("2", I18N.message("completed"));
		map.put("3", I18N.message("canceled"));

		select.setValueMap(map);
		select.setWidth(90);
		return select;
	}

	public static SelectItem newEmailProtocolSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));
		select.setWidth(110);
		select.setValueMap("pop3", "imap");
		return select;
	}

	public static SelectItem newEmailFolderingSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));
		select.setWidth(110);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("none"));
		map.put("1", I18N.message("year"));
		map.put("2", I18N.message("month"));
		map.put("3", I18N.message("day"));
		select.setValueMap(map);
		return select;
	}

	public static SelectItem newEffectSelector(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));
		select.setWidth(110);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("", "");
		map.put("move", I18N.message("move"));
		map.put("copy", I18N.message("copy"));
		select.setValueMap(map);
		return select;
	}

	public static SelectItem newEmailFields(String name, String title) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message(title));
		select.setWidth(110);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("subject"));
		map.put("1", I18N.message("sender"));
		map.put("2", I18N.message("content"));
		map.put("3", I18N.message("recipient"));
		select.setValueMap(map);
		return select;
	}

	public static SelectItem newAliasTypeSelector() {
		SelectItem item = new SelectItem();
		item.setName("aliastype");
		item.setTitle(I18N.message("type"));
		item.setWrapTitle(false);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("", I18N.message("document"));
		map.put("conversion.pdf", I18N.message("pdfconversion"));

		item.setValueMap(map);
		return item;
	}

	public static SelectItem newArchiveSelector(int mode, Integer status) {
		SelectItem item = new SelectItem("archive");
		item.setTitle("");
		item.setRequiredMessage(I18N.message("fieldrequired"));
		ListGridField name = new ListGridField("name", I18N.message("name"));
		ListGridField description = new ListGridField("description", I18N.message("description"));
		item.setValueField("id");
		item.setDisplayField("name");
		item.setPickListWidth(300);
		item.setPickListFields(name, description);
		item.setOptionDataSource(new ArchivesDS(mode, null, status, null));
		if (!Feature.enabled(Feature.IMPEX))
			item.setDisabled(true);
		return item;
	}

	public static SelectItem newImportCustomIds() {
		SelectItem item = newSelectItem("importcids", null);

		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put(Integer.toString(GUIArchive.CUSTOMID_NOT_IMPORT), I18N.message("ignore"));
		map.put(Integer.toString(GUIArchive.CUSTOMID_IMPORT_AND_NEW_RELEASE), I18N.message("importasnewversion"));
		map.put(Integer.toString(GUIArchive.CUSTOMID_IMPORT_AND_NEW_SUBVERSION), I18N.message("importasnewsubversion"));
		map.put(Integer.toString(GUIArchive.CUSTOMID_IMPORT_AND_NEW_DOCUMENT), I18N.message("importasnewdoc"));
		item.setValueMap(map);
		return item;
	}

	public static SelectItem newWorkflowSelector(Long userId) {
		return newWorkflowSelector(userId, true);
	}

	public static SelectItem newWorkflowSelector(Long userId, boolean deployedOnly) {
		SelectItem item = new SelectItem("workflow", I18N.message("workflow"));
		item.setShowHintInField(true);
		item.setHint(I18N.message("workflowselect") + "...");
		item.setRequiredMessage(I18N.message("fieldrequired"));
		ListGridField label = new ListGridField("label", I18N.message("workflow"));
		label.setWidth(150);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(150);
		name.setHidden(true);

		ListGridField description = new ListGridField("description", I18N.message("description"));
		description.setWidth(500);

		item.setWidth(250);
		item.setPickListWidth(500);
		item.setPickListFields(label, description, name);
		item.setDisplayField("label");
		item.setValueField("id");
		item.setWrapTitle(false);
		item.setOptionDataSource(new WorkflowsDS(false, deployedOnly, userId));
		if (!Feature.enabled(Feature.WORKFLOW))
			item.setDisabled(true);
		return item;
	}

	public static ComboBoxItem newWorkflowCombo(Long userId) {
		ComboBoxItem item = new ComboBoxItem("workflow", I18N.message("workflow"));
		item.setShowHintInField(true);
		item.setHint(I18N.message("workflowselect") + "...");
		item.setWidth(200);
		item.setWrapTitle(false);
		ListGridField name = new ListGridField("name");
		ListGridField label = new ListGridField("label", I18N.message("workflow"));
		item.setDisplayField("label");
		item.setValueField("id");
		item.setPickListWidth(300);
		item.setPickListFields(label, name);
		item.setOptionDataSource(new WorkflowsDS(false, false, userId));
		return item;
	}

	public static SelectItem newJobGroupSelector() {
		SelectItem item = new SelectItem("group", I18N.message("group"));
		item.setRequiredMessage(I18N.message("fieldrequired"));
		item.setShowHintInField(true);
		item.setHint(I18N.message("groupselect") + "...");
		ListGridField group = new ListGridField("group", I18N.message("group"));
		item.setWidth(200);
		item.setPickListFields(group);
		item.setDisplayField("group");
		item.setValueField("group");
		item.setWrapTitle(false);
		item.setOptionDataSource(new JobsDS());
		return item;
	}

	public static SelectItem newFormSelector() {
		SelectItem item = new SelectItem("form", I18N.message("form"));
		item.setRequiredMessage(I18N.message("fieldrequired"));
		ListGridField name = new ListGridField("name", I18N.message("name"));
		item.setPickListFields(name);
		item.setDisplayField("name");
		item.setValueField("id");
		item.setWrapTitle(false);
		item.setOptionDataSource(new FormsDS());
		if (!Feature.enabled(Feature.FORM))
			item.setDisabled(true);
		return item;
	}

	public static SelectItem newStampSelector() {
		SelectItem item = new SelectItem("stamp", I18N.message("stamp"));
		item.setRequiredMessage(I18N.message("fieldrequired"));
		ListGridField name = new ListGridField("name", I18N.message("name"));
		item.setWidth(200);
		item.setPickListWidth(200);
		item.setPickListFields(name);
		item.setDisplayField("name");
		item.setValueField("id");
		item.setWrapTitle(false);
		item.setOptionDataSource(new StampsDS(Session.get().getUser().getId(), true));
		if (!Feature.enabled(Feature.STAMP))
			item.setDisabled(true);
		return item;
	}

	public static Label newLinkLabel(String title) {
		Label label = new Label(I18N.message(title));
		label.setWrap(false);
		label.setCursor(Cursor.HAND);
		label.setAutoWidth();
		return label;
	}

	/**
	 * Creates a new FloatItem.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static FloatItem newFloatItem(String name, String title, Float value) {
		FloatItem item = new FloatItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		IsFloatValidator iv = new IsFloatValidator();
		iv.setErrorMessage(I18N.message("wholenumber"));
		item.setValidators(iv);
		return item;
	}

	/**
	 * Creates a new FloatItem for the Extended AttributesDS.
	 * 
	 * @param name The item name (mandatory)
	 * @param label The item label (optional)
	 * @param value The item value (optional)
	 * 
	 * @return the new item
	 */
	public static FormItem newFloatItemForAttribute(String name, String label, Float value) {
		// We cannot use spaces in items name
		String itemName = itemNameForAttribute(name);
		FormItem item = newFloatItem(itemName, label, value);
		if (!InputValues.getInputs(item.getName()).isEmpty()) {
			item = formItemWithSuggestions(item);
			IsFloatValidator iv = new IsFloatValidator();
			iv.setErrorMessage(I18N.message("wholenumber"));
			item.setValidators(iv);
		}
		return item;
	}

	private static FormItem formItemWithSuggestions(FormItem srcItem) {
		FormItem item = null;
		item = new ComboBoxItem(srcItem.getName(), srcItem.getTitle());
		item.setValueMap(InputValues.getInputsAsStrings(srcItem.getName()).toArray(new String[0]));
		item.setShowPickerIcon(false);
		item.setTextBoxStyle("textItemLite");
		if (srcItem.getValue() != null)
			item.setValue(srcItem.getValue());
		return item;
	}

	/**
	 * Simple yes/no radio button. yes=true, no=false
	 * 
	 * @param name name of the item
	 * @param label label of the item(optional)
	 * 
	 * @return the new item
	 */
	public static RadioGroupItem newYesNoRadioItem(String name, String label) {
		RadioGroupItem item = new RadioGroupItem(filterItemName(name), I18N.message(label));
		item.setVertical(false);
		item.setShowTitle(true);
		item.setWrap(false);
		item.setWrapTitle(false);

		LinkedHashMap<String, String> values = new LinkedHashMap<String, String>();
		values.put("true", I18N.message("yes"));
		values.put("false", I18N.message("no"));
		item.setValueMap(values);
		item.setValue("true");

		return item;
	}

	public static SelectItem newTagInputMode(String name, String title) {
		SelectItem mode = new SelectItem();
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("free", I18N.message("free"));
		opts.put("preset", I18N.message("preset"));
		mode.setValueMap(opts);
		mode.setName(filterItemName(name));
		if (title != null)
			mode.setTitle(I18N.message(title));
		else
			mode.setShowTitle(false);
		mode.setWrapTitle(false);
		mode.setDefaultValue("free");
		mode.setWidth(150);
		return mode;
	}

	public static SelectItem newRunlevelSelector() {
		SelectItem item = new SelectItem("runlevel", I18N.message("currentrunlevel"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("default", I18N.message("default"));
		opts.put("bulkload", I18N.message("bulkload"));
		opts.put("slave", I18N.message("slave"));
		opts.put("devel", I18N.message("devel"));
		item.setValueMap(opts);
		item.setWrapTitle(false);
		item.setValue(Session.get().getConfig("runlevel"));
		item.setRequired(true);
		item.setWidth(150);
		item.setDisabled("demo".equals(Session.get().getConfig("runlevel")));
		return item;
	}

	public static SelectItem newFolderTemplateSelector() {
		SelectItem item = new SelectItem("foldertemplate");
		item.setTitle(I18N.message("foldertemplate"));
		item.setRequiredMessage(I18N.message("fieldrequired"));
		item.setValueField("id");
		item.setDisplayField("name");
		item.setWrapTitle(false);
		item.setOptionDataSource(new FolderTemplatesDS());
		if (!Feature.enabled(Feature.FOLDER_TEMPLATE))
			item.setDisabled(true);
		return item;
	}

	public static SelectItem newWorkspaceSelector(Long value) {
		SelectItem item = new SelectItem("workspace");
		item.setTitle(I18N.message("workspace"));
		item.setRequiredMessage(I18N.message("fieldrequired"));
		item.setValueField("folderId");
		item.setDisplayField("name");
		item.setWrapTitle(false);
		item.setOptionDataSource(new FoldersDS("profile-workspace", true));
		item.setValue(value);
		item.setVisible(Feature.enabled(Feature.MULTI_WORKSPACE));
		item.setWidth(150);
		return item;
	}

	public static SelectItem newRetentionDateOption(int value) {
		SelectItem selector = new SelectItem("dateoption", I18N.message("dateoption"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("" + GUIRetentionPolicy.DATE_OPT_CREATION, I18N.message("created"));
		opts.put("" + GUIRetentionPolicy.DATE_OPT_PUBLISHED, I18N.message("published"));
		opts.put("" + GUIRetentionPolicy.DATE_OPT_STOPPUBLISHING, I18N.message("stoppublishing"));
		opts.put("" + GUIRetentionPolicy.DATE_OPT_ARCHIVED, I18N.message("archiveds"));
		selector.setValueMap(opts);

		selector.setWrapTitle(false);
		selector.setWidth(150);

		selector.setValue("" + value);
		selector.setDefaultValue("" + value);

		return selector;
	}

	public static SelectItem newImportFolderProviderOption(String value) {
		SelectItem selector = new SelectItem("provider", I18N.message("type"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		if (Feature.enabled(Feature.IMPORT_LOCAL_FOLDERS))
			opts.put(GUIImportFolder.PROVIDER_FILE, I18N.message("localfolder"));
		if (Feature.enabled(Feature.IMPORT_REMOTE_FOLDERS)) {
			opts.put(GUIImportFolder.PROVIDER_SMB, I18N.message("smbshare"));
			opts.put(GUIImportFolder.PROVIDER_SMB2, I18N.message("smb2share"));
			opts.put(GUIImportFolder.PROVIDER_SMB3, I18N.message("smb3share"));
			opts.put(GUIImportFolder.PROVIDER_FTP, I18N.message("fftp"));
			opts.put(GUIImportFolder.PROVIDER_FTPS, I18N.message("ftps"));
			opts.put(GUIImportFolder.PROVIDER_SFTP, I18N.message("sftp"));
		}
		selector.setValueMap(opts);

		selector.setWrapTitle(false);
		selector.setWidth(150);

		selector.setValue(value);
		selector.setDefaultValue(value);

		return selector;
	}

	public static SelectItem newDocuSignTabType(String value) {
		SelectItem selector = new SelectItem("type", I18N.message("type"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("esig-signhere", I18N.message("annotation.type.esig-signhere"));
		opts.put("esig-text", I18N.message("annotation.type.esig-text"));
		opts.put("esig-date", I18N.message("annotation.type.esig-date"));
		opts.put("esig-company", I18N.message("annotation.type.esig-company"));
		opts.put("esig-datesigned", I18N.message("annotation.type.esig-datesigned"));
		opts.put("esig-title", I18N.message("annotation.type.esig-title"));
		opts.put("esig-fullname", I18N.message("annotation.type.esig-fullname"));
		opts.put("esig-email", I18N.message("annotation.type.esig-email"));
		opts.put("esig-emailaddress", I18N.message("annotation.type.esig-emailaddress"));
		opts.put("esig-envelopeid", I18N.message("annotation.type.esig-envelopeid"));

		selector.setValueMap(opts);
		selector.setWrapTitle(false);
		selector.setValue(value);
		selector.setDefaultValue(value);
		return selector;
	}

	public static SelectItem newRetentionAction(int value) {
		SelectItem selector = new SelectItem("action", I18N.message("action"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("" + GUIRetentionPolicy.ACTION_ARCHIVE, I18N.message("archive"));
		opts.put("" + GUIRetentionPolicy.ACTION_UNPUBLISH, I18N.message("unpublish"));
		opts.put("" + GUIRetentionPolicy.ACTION_DELETE, I18N.message("ddelete"));
		selector.setValueMap(opts);

		selector.setWrapTitle(false);
		selector.setWidth(150);

		selector.setValue("" + value);
		selector.setDefaultValue("" + value);

		return selector;
	}

	public static SelectItem new2AFMethodSelector(String name, String value, boolean enabledOnly) {
		SelectItem select = new SelectItem(filterItemName(name), I18N.message("authenticationmethod"));
		select.setWidth(250);
		select.setWrapTitle(false);
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("", I18N.message("disable2fa"));
		if (!enabledOnly
				|| Session.get().getTenantConfigAsBoolean("2fa." + Constants.TWOFA_GOOGLE_AUTHENTICATOR + ".enabled"))
			map.put(Constants.TWOFA_GOOGLE_AUTHENTICATOR, "Google Authenticator");
		if (!enabledOnly || Session.get().getTenantConfigAsBoolean("2fa." + Constants.TWOFA_YUBIKEY + ".enabled"))
			map.put(Constants.TWOFA_YUBIKEY, "Yubikey");
		if (!enabledOnly
				|| Session.get().getTenantConfigAsBoolean("2fa." + Constants.TWOFA_EMAIL_AUTHENTICATOR + ".enabled"))
			map.put(Constants.TWOFA_EMAIL_AUTHENTICATOR, "Email");
		if (!enabledOnly || Session.get().getTenantConfigAsBoolean("2fa." + Constants.TWOFA_DUO + ".enabled"))
			map.put(Constants.TWOFA_DUO, "Duo");
		select.setValueMap(map);
		if (value != null && map.get(value) != null)
			select.setValue(value.toString());
		else
			select.setValue("");
		return select;
	}

	public static SelectItem newSplittingPolicySelector() {
		SelectItem select = new SelectItem("splittingpolicy", I18N.message("splittingpolicy"));
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("allpages"));
		map.put("1", I18N.message("selectionofpages"));
		map.put("2", I18N.message("blankpageasseparator"));
		map.put("3", I18N.message("barcodeasseparator"));
		map.put("4", I18N.message("textasseparator"));
		select.setValueMap(map);
		select.setValue("0");
		select.setWidth(200);
		return select;
	}

	public static SelectItem newSplitSeparatorHandlingSelector() {
		SelectItem select = new SelectItem("separatorhandling", I18N.message("separatorhandling"));
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("0", I18N.message("skip"));
		map.put("1", I18N.message("currentsegment"));
		map.put("2", I18N.message("nextsegment"));
		select.setValueMap(map);
		select.setValue("0");
		select.setWidth(200);
		return select;
	}

	public static SelectItem newSubscriptionTypeSelector() {
		SelectItem selector = new SelectItem("subscriptionType", I18N.message("type"));
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("document", I18N.message("document").toLowerCase());
		opts.put("folder", I18N.message("folder").toLowerCase());
		selector.setValueMap(opts);
		selector.setRequired(false);

		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			@Override
			public void onFormItemClick(FormItemIconClickEvent event) {
				selector.clearValue();
				selector.setValue((String) null);
				selector.fireEvent(new ChangedEvent(selector.getJsObj()));
			}
		});
		clear.setWidth(12);
		clear.setHeight(12);
		selector.setIcons(clear);

		return selector;
	}

	/**
	 * Creates a new TextAreaItem suitable for notes.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * @param handler The changed handler (optional)
	 * @param withSimplifiedHtmlEditor If the icon that opens the simplified
	 *        HTML editor must be shown
	 * 
	 * @return the text item
	 */
	public static TextAreaItem newTextAreaItemForNote(String name, String title, String value, ChangedHandler handler,
			boolean withSimplifiedHtmlEditor) {
		TextAreaItem item = newTextAreaItem(name, title, value);
		if (withSimplifiedHtmlEditor) {
			FormItemIcon editHtml = new FormItemIcon();
			editHtml.setName("edithtml");
			editHtml.setWidth(16);
			editHtml.setHeight(16);
			editHtml.setSrc("[SKIN]/html.png");
			editHtml.setPrompt(I18N.message("openhtmleditor"));
			editHtml.addFormItemClickHandler(new FormItemClickHandler() {

				@Override
				public void onFormItemClick(FormItemIconClickEvent event) {
					RichTextItem htmlItem = newRichTextItemForNote("html", "html", item.getValueAsString());
					htmlItem.setBrowserSpellCheck(true);
					htmlItem.setShowTitle(false);
					if (handler != null)
						htmlItem.addChangedHandler(handler);
					LD.askForValue("edithtml", null, item.getValueAsString(), htmlItem, new ValueCallback() {

						@Override
						public void execute(String value) {
							item.setValue(value);
							if (handler != null)
								handler.onChanged(null);
						}
					});
				}
			});

			item.setIcons(editHtml);
			item.setIconVAlign(VerticalAlignment.CENTER);
		}

		if (handler != null)
			item.addChangedHandler(handler);
		addNoteValidator(item);
		return item;
	}

	/**
	 * Creates a new RichTextItem suitable for notes.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * 
	 * @return the text item
	 */
	public static RichTextItem newRichTextItemForNote(String name, String title, String value) {
		RichTextItem item = new RichTextItem();
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		else
			item.setValue("");
		item.setWrapTitle(false);
		item.setRequiredMessage(I18N.message("fieldrequired"));
		item.setShowTitle(false);
		item.setRequired(true);
		item.setWidth("*");
		item.setHeight("*");

		addNoteValidator(item);
		return item;
	}

	private static void addNoteValidator(FormItem item) {
		int maxlength = Session.get().getConfigAsInt("default.gui.note.maxlength");
		if (maxlength > 0)
			item.setValidators(new CustomValidator() {

				@Override
				protected boolean condition(Object value) {
					if (getFormItem().getRequired() && (value == null || value.toString().length() < 1)) {
						setErrorMessage(I18N.message("fieldrequired"));
						return false;
					}

					setErrorMessage(I18N.message("contentexceedsmax", Integer.toString(maxlength)));
					if (value != null && value.toString().length() > maxlength) {
						return false;
					}
					return true;
				}
			});
	}

	/**
	 * Creates a new RichTextItem suitable for emails.
	 * 
	 * @param name The item name (mandatory)
	 * @param title The item title (mandatory)
	 * @param value The item value (optional)
	 * @param chagnedHandler A handler thas is notified about changes (optional)
	 * 
	 * @return the text item
	 */
	public static RichTextItem newRichTextItemForEmail(String name, String title, String value,
			ChangedHandler chagnedHandler) {
		RichTextItem item = newRichTextItemForAutomation(name, title, value, chagnedHandler);
		item.setName(filterItemName(name));
		item.setTitle(I18N.message(title));
		if (value != null)
			item.setValue(value);
		else
			item.setValue("");

		int maxlength = Session.get().getConfigAsInt("default.gui.email.maxlength");
		if (maxlength > 0)
			item.setValidators(new CustomValidator() {

				@Override
				protected boolean condition(Object value) {
					if (getFormItem().getRequired() && (value == null || value.toString().length() < 1)) {
						setErrorMessage(I18N.message("fieldrequired"));
						return false;
					}

					setErrorMessage(I18N.message("contentexceedsmax", Integer.toString(maxlength)));
					if (value != null && value.toString().length() > maxlength) {
						return false;
					}
					return true;
				}
			});
		return item;
	}

	/**
	 * Filter the name from problematic chars
	 * 
	 * @param name name of the item
	 * 
	 * @return sanitized name
	 */
	public static String filterItemName(String name) {
		return name.toString().replace("\\.", "_");
	}

	/**
	 * Obtain the original item name
	 * 
	 * @param name sanitized name
	 * 
	 * @return original name
	 */
	public static String originalItemName(String name) {
		return name.toString().replace("_", "\\.");
	}

	public static String itemNameForAttribute(String name) {
		return "_" + name.replace(" ", Constants.BLANK_PLACEHOLDER);
	}
}