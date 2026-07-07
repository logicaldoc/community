package com.logicaldoc.core.communication;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of {@link MessageTemplateDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
@Repository("messageTemplateDAO")
@Transactional
public class HibernateMessageTemplateDAO extends HibernatePersistentObjectDAO<MessageTemplate>
        implements MessageTemplateDAO {

    private static final String LANGUAGE = "language";
    
    private static final String TENANT_ID = "tenantId";

    public HibernateMessageTemplateDAO() {
        super(MessageTemplate.class);
        super.log = LoggerFactory.getLogger(HibernateMessageTemplateDAO.class);
    }

    @Override
    public List<MessageTemplate> findByLanguage(String language, long tenantId) throws PersistenceException {
        return findByWhere("_entity.language = :language and _entity.tenantId = :tenantId",
                Map.of(LANGUAGE, language, TENANT_ID, tenantId), "_entity.name", null);
    }

    @Override
    public List<MessageTemplate> findByTypeAndLanguage(MessageTemplate.Type type, String language, long tenantId)
            throws PersistenceException {
        Map<String, Object> params = new HashMap<>();
        params.put(TENANT_ID, tenantId);

        StringBuilder query = new StringBuilder("_entity.tenantId = :tenantId");

        if (StringUtils.isNotEmpty(language)) {
            query.append(" and _entity.language = :language");
            params.put(LANGUAGE, language);
        }

        if (type != null) {
            query.append(" and _entity.type = :type ");
            params.put("type", type);
        }

        return findByWhere(query.toString(), params, "_entity.name", null);
    }

    @Override
    public MessageTemplate findByNameAndLanguage(String name, String language, long tenantId)
            throws PersistenceException {
        String lang = StringUtils.defaultIfEmpty(language, "en");

        String query = "_entity.language = :language and _entity.name = :name and _entity.tenantId = :tenantId";
        List<MessageTemplate> buf = findByWhere(query, Map.of(LANGUAGE, lang, "name", name, TENANT_ID, tenantId),
                null, null);
        if (CollectionUtils.isNotEmpty(buf))
            return buf.get(0);

        buf = findByWhere(query, Map.of(LANGUAGE, "en", "name", name, TENANT_ID, tenantId), null, null);
        if (CollectionUtils.isNotEmpty(buf))
            return buf.get(0);

        return null;
    }

    @Override
    public void delete(long id, int code) throws PersistenceException {
        if (!checkStoringAspect())
            return;

        if (code == 0)
            throw new IllegalArgumentException("code cannot be 0");

        MessageTemplate template = findById(id);
        if (template != null) {
            template.setDeleted(code);
            template.setName("%s.%d".formatted(template.getName(), template.getId()));
            saveOrUpdate(template);
        }
    }

    @Override
    public List<MessageTemplate> findByName(String name, long tenantId) throws PersistenceException {
        return findByWhere("_entity.name = :name and _entity.tenantId = :tenantId",
                Map.of("name", name, TENANT_ID, tenantId), null, null);
    }
}