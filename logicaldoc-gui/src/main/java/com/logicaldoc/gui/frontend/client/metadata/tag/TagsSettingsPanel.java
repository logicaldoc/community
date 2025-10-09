package com.logicaldoc.gui.frontend.client.metadata.tag;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows some general settings for the tags management.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class TagsSettingsPanel extends VLayout {

	private static final String MINSIZE = "minsize";

	private static final String MAXSIZE = "maxsize";

	private static final String CLOUD_ELEMENTS = "cloudElements";

	private static final String SELECT_ELEMENTS = "selectElements";

	private ValuesManager vm = new ValuesManager();

	private List<GUIParameter> settings;

	public TagsSettingsPanel(List<GUIParameter> settings) {
		this.settings = settings;
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		DynamicForm parametersForm = new DynamicForm();
		parametersForm.setValuesManager(vm);
		parametersForm.setTitleOrientation(TitleOrientation.LEFT);
		parametersForm.setNumCols(2);
		parametersForm.setColWidths(1, "*");
		parametersForm.setPadding(5);

		SelectItem mode = ItemFactory.newTagInputMode("mode", "inputmode");

		SpinnerItem maxsize = ItemFactory.newSpinnerItem(MAXSIZE, (Long) null);
		maxsize.setRequired(true);

		SpinnerItem minsize = ItemFactory.newSpinnerItem(MINSIZE, (Long) null);
		minsize.setRequired(true);

		SpinnerItem selectElements = ItemFactory.newSpinnerItem(SELECT_ELEMENTS, I18N.message("tagselectelements"),
				(Long) null);
		selectElements.setRequired(true);
		selectElements.setWrapTitle(false);

		SpinnerItem cloudElements = ItemFactory.newSpinnerItem(CLOUD_ELEMENTS, I18N.message("tagcloudelements"),
				(Long) null);
		cloudElements.setRequired(true);
		cloudElements.setWrapTitle(false);

		TextItem vocabulary = ItemFactory.newTextItem("vocabulary", null);
		vocabulary.setRequired(true);
		vocabulary.setWidth(300);

		parametersForm.setItems(mode, maxsize, minsize, selectElements, cloudElements, vocabulary);
		addMember(parametersForm);

		for (GUIParameter p : settings) {
			if (p.getName().endsWith("tag.mode"))
				mode.setValue(p.getValue());
			if (p.getName().endsWith("tag.maxsize"))
				maxsize.setValue(p.getValue());
			if (p.getName().endsWith("tag.minsize"))
				minsize.setValue(p.getValue());
			if (p.getName().endsWith("tag.vocabulary"))
				vocabulary.setValue(p.getValue());
			if (p.getName().endsWith("tagcloud.maxtags"))
				cloudElements.setValue(p.getValue());
			if (p.getName().endsWith("tag.select.maxtags"))
				selectElements.setValue(p.getValue());
		}

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				final Map<String, Object> values = vm.getValues();

				if (Boolean.TRUE.equals(vm.validate())) {
					List<GUIParameter> params = new ArrayList<>();

					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.mode",
							values.get("mode").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.maxsize",
							values.get(MAXSIZE).toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.minsize",
							values.get(MINSIZE).toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tag.vocabulary",
							values.get("vocabulary").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tagcloud.maxtags",
							values.get(CLOUD_ELEMENTS).toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".tag.select.maxtags",
							values.get(SELECT_ELEMENTS).toString()));

					SettingService.Instance.get().saveSettings(params, new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(Void ret) {
							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".tag.mode",
									values.get("mode").toString());
							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".tag.maxsize",
									values.get(MAXSIZE).toString());

							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".tag.minsize",
									values.get(MINSIZE).toString());

							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".tagcloud.maxtags",
									values.get(CLOUD_ELEMENTS).toString());

							Session.get().getInfo().setConfig(Session.get().getTenantName() + ".tag.select.maxtags",
									values.get(SELECT_ELEMENTS).toString());

							GuiLog.info(I18N.message("settingssaved"), null);
						}
					});
				}
			}
		});

		addMember(save);
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