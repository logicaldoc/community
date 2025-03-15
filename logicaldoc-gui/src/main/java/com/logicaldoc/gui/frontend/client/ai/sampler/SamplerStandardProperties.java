package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows sampler's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerStandardProperties extends SamplerDetailsTab {

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector folderSelector;

	public SamplerStandardProperties(GUISampler sampler, final ChangedHandler changedHandler) {
		super(sampler, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		folderSelector = new FolderSelector("source", null);
		folderSelector.setRequired(true);
		folderSelector.setWidth(250);
		folderSelector.setTitle(I18N.message("sourcefolder"));
		if (sampler.getFolder() != null)
			folderSelector.setFolder(sampler.getFolder());
		folderSelector.addFolderChangeListener(folder -> changedHandler.onChanged(null));

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(formsContainer.contains(form)))
			formsContainer.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem name = ItemFactory.newSimpleTextItem("name", sampler.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);

		TextItem label = ItemFactory.newTextItem("label", sampler.getLabel());
		label.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", sampler.getDescription());
		description.addChangedHandler(changedHandler);

		SelectItem type = ItemFactory.newSelectItem("type");
		type.setOptionDataSource(new SamplerTypeDS());
		type.setValueField("value");
		type.setDisplayField("value");
		type.setValue(sampler.getType());
		type.addChangedHandler(changedHandler);
		type.setRequired(true);

		form.setItems(name, type, label, folderSelector);

		formsContainer.addMember(form);

	}

	boolean validate() {
		if (form.validate()) {
			sampler.setName(form.getValueAsString("name"));
			sampler.setLabel(form.getValueAsString("label"));
			sampler.setDescription(form.getValueAsString("description"));
			sampler.setType(form.getValueAsString("type"));
			sampler.setFolder(folderSelector.getFolder());
		}
		return !form.hasErrors();
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