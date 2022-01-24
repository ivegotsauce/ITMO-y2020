package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.repository.TalkRepository;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class TalkRepositoryImpl extends BasicRepositoryImpl implements TalkRepository {
    @Override
    public List<List<Object>> findByUserId(long userId) { // DTO
        List<?> list = super.findImpl("SELECT Talk.*, u1.login AS sourceUserLogin, u2.login AS targetUserLogin FROM (`Talk` JOIN `User` AS u1 ON (Talk.sourceUserId=u1.id)) JOIN `User` AS u2 ON (Talk.targetUserId=u2.id) WHERE (Talk.sourceUserId=? OR Talk.targetUserId=?) ORDER BY Talk.creationTime",
                ArrayList.class, userId, userId);
        List<List<Object>> talks = new ArrayList<>();
        for (Object obj : list) {
            List<?> impl = (List<?>) obj;
            List<Object> objects = new ArrayList<>(impl);
            talks.add(objects);
        }
        return talks;
    }



    protected  <T> T toBasicType(ResultSetMetaData metaData, ResultSet resultSet, Class<T> tClass) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Talk talk = new Talk();
        String sourceUserLogin = "";
        String targetUserLogin = "";
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnLabel(i)) {
                case "id":
                    talk.setId(resultSet.getLong(i));
                    break;
                case "sourceUserId":
                    talk.setSourceUserId(resultSet.getLong(i));
                    break;
                case "targetUserId":
                    talk.setTargetUserId(resultSet.getLong(i));
                    break;
                case "text":
                    talk.setText(resultSet.getString(i));
                    break;
                case "creationTime":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "sourceUserLogin":
                    sourceUserLogin = resultSet.getString(i);
                    break;
                case "targetUserLogin":
                    targetUserLogin = resultSet.getString(i);
                    break;
                default:
                    // No operations.
            }
        }
        List<Object> util = new ArrayList<>();
        util.add(talk);
        util.add(sourceUserLogin);
        util.add(targetUserLogin);
        return (T) util;
    }


    @Override
    public void save(Talk talk) {
        super.save(talk);
    }

    @Override
    protected <T> void setEnteredFields(PreparedStatement statement, T t, String... args) throws SQLException {
        Talk talk = (Talk) t;
        statement.setLong(1, talk.getSourceUserId());
        statement.setLong(2, talk.getTargetUserId());
        statement.setString(3, talk.getText());
    }

    @Override
    protected <T> void setGeneratedFields(ResultSet generatedKeys, T t) throws SQLException {
        Talk talk = (Talk) t;
        talk.setId(generatedKeys.getLong(1));
    }

    @Override
    protected String getInsertQueryTemplate() {
        return "INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `text`, `creationTime`) VALUES (?, ?, ?, NOW())";
    }
}
