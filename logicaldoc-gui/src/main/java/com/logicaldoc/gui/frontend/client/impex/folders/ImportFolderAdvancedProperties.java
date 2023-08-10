package com.logicaldoc.gui.frontend.client.impex.folders;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows import folder's advanced properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportFolderAdvancedProperties extends ImportFolderDetailsTab {
	private static final String PREVENTDUPLICATIONS = "preventduplications";

	private static final String BARCODETEMPLATE = "barcodetemplate";

	private static final String OCRTEMPLATE = "ocrtemplate";

	private static final String TEMPLATE = "template";

	private static final String SIZEMAX = "sizemax";

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	public ImportFolderAdvancedProperties(GUIImportFolder importFolder, ChangedHandler changedHandler) {
		super(importFolder, changedHandler);
		setWidth100();
		setHeight100();
		setMembers(formsContainer);
		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(formsContainer.contains(form)))
			formsContainer.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(3);
		form.setTitleOrientation(TitleOrientation.TOP);

		SpinnerItem depth = ItemFactory.newSpinnerItem("depth", importFolder.getDepth());
		depth.setRequired(true);
		depth.setWidth(60);
		depth.addChangedHandler(changedHandler);

		IntegerItem size = ItemFactory.newIntegerItem(SIZEMAX, SIZEMAX, importFolder.getMaxSize());
		size.addChangedHandler(changedHandler);
		size.setHint("KB");
		size.setWidth(100);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);
		template.addChangedHandler(changedHandler);
		template.setMultiple(false);
		if (importFolder.getTemplateId() != null)
			template.setValue(importFolder.getTemplateId().toString());

		ChangedHandler changeTemplateHandler = event -> {
				if (form.getValue(TEMPLATE) == null || "".equals(form.getValueAsString(TEMPLATE)))
					importFolder.setTemplateId(null);
				else
					importFolder.setTemplateId(Long.parseLong(form.getValueAsString(TEMPLATE)));
				importFolder.setOcrTemplateId(null);
				importFolder.setBarcodeTemplateId(null);
				refresh();
		};
		template.addChangedHandler(changeTemplateHandler);

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, importFolder.getTemplateId(),
				importFolder.getOcrTemplateId());
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.setMultiple(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) || importFolder.getTemplateId() == null);

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, importFolder.getTemplateId(),
				importFolder.getBarcodeTemplateId());
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.setMultiple(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));

		CheckboxItem delImport = new CheckboxItem();
		delImport.setName("delImport");
		delImport.setTitle(I18N.message("deleteafterimport"));
		delImport.setRedrawOnChange(true);
		delImport.setWidth(50);
		delImport.setValue(importFolder.isDelImport());
		delImport.addChangedHandler(changedHandler);

		CheckboxItem importEmpty = new CheckboxItem();
		importEmpty.setName("importEmpty");
		importEmpty.setTitle(I18N.message("importemptyfolders"));
		importEmpty.setRedrawOnChange(true);
		importEmpty.setWidth(50);
		importEmpty.setValue(importFolder.isImportEmpty());
		importEmpty.addChangedHandler(changedHandler);

		CheckboxItem inheritRights = new CheckboxItem();
		inheritRights.setName("inheritRights");
		inheritRights.setTitle(I18N.message("inheritrights"));
		inheritRights.setRedrawOnChange(true);
		inheritRights.setWidth(50);
		inheritRights.setValue(importFolder.isInheritRights());
		inheritRights.addChangedHandler(changedHandler);

		CheckboxItem preventDuplications = new CheckboxItem();
		preventDuplications.setName(PREVENTDUPLICATIONS);
		preventDuplications.setTitle(I18N.message(PREVENTDUPLICATIONS));
		preventDuplications.setRedrawOnChange(true);
		preventDuplications.setWidth(50);
		preventDuplications.setValue(importFolder.isPreventDuplications());
		preventDuplications.addChangedHandler(changedHandler);

		TextItem tags = ItemFactory.newTextItem("tags", importFolder.getTags());
		tags.addChangedHandler(changedHandler);

		final DateItem startDate = ItemFactory.newDateItem("startdate", "earliestdate");
		startDate.addChangedHandler(changedHandler);
		startDate.setValue(importFolder.getStartDate());
		startDate.setUseMask(false);
		startDate.setShowPickerIcon(true);
		startDate.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
		startDate.addKeyPressHandler(event -> {
			if ("delete".equals(event.getKeyName().toLowerCase())) {
				startDate.clearValue();
				startDate.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});

		SelectItem updatePolicy = ItemFactory.newSelectItem("updatePolicy", "onupdate");
		updatePolicy.addChangedHandler(changedHandler);
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("0", I18N.message("createnewversion"));
		map.put("1", I18N.message("createnewdoc"));
		updatePolicy.setValueMap(map);
		updatePolicy.setValue(Integer.toString(importFolder.getUpdatePolicy()));

		form.setItems(depth, size, startDate, template, ocrTemplate, barcodeTemplate, tags, updatePolicy, importEmpty,
				preventDuplications, inheritRights, delImport);

		formsContainer.addMember(form);

	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = form.getValues();
		if (!form.validate())
			return false;

		collectSizeMax(values);

		importFolder.setDepth(Integer.parseInt(values.get("depth").toString()));
		importFolder.setUpdatePolicy(Integer.parseInt(values.get("updatePolicy").toString()));

		collectTemplates(values);

		importFolder.setDelImport((Boolean) values.get("delImport"));
		importFolder.setInheritRights((Boolean) values.get("inheritRights"));
		importFolder.setImportEmpty((Boolean) values.get("importEmpty"));
		importFolder.setPreventDuplications((Boolean) values.get(PREVENTDUPLICATIONS));

		collectTags(values);

		importFolder.setStartDate((Date) values.get("startdate"));

		return !form.hasErrors();
	}

	private void collectTags(Map<String, Object> values) {
		if (values.get("tags") != null || !"".equals(values.get("tags")))
			importFolder.setTags((String) values.get("tags"));
		else
			importFolder.setTags(null);
	}

	private void collectTemplates(Map<String, Object> values) {
		if (values.get(TEMPLATE) == null || "".equals(values.get(TEMPLATE)))
			importFolder.setTemplateId(null);
		else
			importFolder.setTemplateId(Long.parseLong((String) values.get(TEMPLATE)));

		if (values.get(OCRTEMPLATE) == null || "".equals(values.get(OCRTEMPLATE)))
			importFolder.setOcrTemplateId(null);
		else
			importFolder.setOcrTemplateId(Long.parseLong((String) values.get(OCRTEMPLATE)));

		if (values.get(BARCODETEMPLATE) == null || "".equals(values.get(BARCODETEMPLATE)))
			importFolder.setBarcodeTemplateId(null);
		else
			importFolder.setBarcodeTemplateId(Long.parseLong((String) values.get(BARCODETEMPLATE)));
	}

	private void collectSizeMax(Map<String, Object> values) {
		if (values.get(SIZEMAX) == null)
			importFolder.setMaxSize(null);
		else if (values.get(SIZEMAX) instanceof Integer)
			importFolder.setMaxSize((Integer) form.getValue(SIZEMAX));
		else
			importFolder.setMaxSize(Integer.parseInt((String) values.get(SIZEMAX)));
	}
}