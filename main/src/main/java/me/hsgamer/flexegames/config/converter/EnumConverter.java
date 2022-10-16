package me.hsgamer.flexegames.config.converter;

public class EnumConverter<T extends Enum<T>> extends StringToObjectConverter<T> {
    private final Class<T> enumClass;

    public EnumConverter(Class<T> enumClass) {
        this.enumClass = enumClass;
    }

    @Override
    public T deserialize(String rawValue) {
        try {
            return Enum.valueOf(enumClass, rawValue.toUpperCase());
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public String serialize(T value) {
        return value.name().toLowerCase();
    }
}
