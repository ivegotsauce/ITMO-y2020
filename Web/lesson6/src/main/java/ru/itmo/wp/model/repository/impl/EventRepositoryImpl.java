package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.repository.EventRepository;

import java.sql.*;

public class EventRepositoryImpl extends BasicRepositoryImpl implements EventRepository {

    @Override
    public void save(Event event) {
        super.save(event);
    }

    @Override
    protected <T> void setEnteredFields(PreparedStatement statement, T t, String... args) throws SQLException {
        Event event = (Event) t;
        statement.setLong(1, event.getUserId());
        statement.setString(2, event.getType().name());
    }

    @Override
    protected <T> void setGeneratedFields(ResultSet generatedKeys, T t) throws SQLException {
        Event event = (Event) t;
        event.setId(generatedKeys.getLong(1));
    }

    @Override
    protected String getInsertQueryTemplate() {
        return "INSERT INTO `Event` (`userId`, `type`, `creationTime`) VALUES (?, ?, NOW())";
    }

    @Override
    protected <T> T toBasicType(ResultSetMetaData metaData, ResultSet resultSet, Class<T> tClass) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Event event= new Event();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    event.setId(resultSet.getLong(i));
                    break;
                case "userId":
                    event.setUserId(resultSet.getLong(i));
                    break;
                case "creationTime":
                    event.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "type":
                    event.setType(resultSet.getString(i).equals("ENTER") ? Event.Type.ENTER : Event.Type.LOGOUT);
                default:
                    // No operations.
            }
        }

        return (T) event;
    }
}
