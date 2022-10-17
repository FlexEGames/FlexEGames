package me.hsgamer.flexegames.config.converter;

import me.hsgamer.hscore.common.Validate;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class NumberObjectMapConverter extends StringObjectMapToObjectConverter<Map<Number, Map<String, Object>>> {
    @Override
    protected Map<Number, Map<String, Object>> deserialize(Map<String, Object> rawValue) {
        Map<Number, Map<String, Object>> map = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : rawValue.entrySet()) {
            List<Number> keys = getNumbers(entry.getKey());
            Map<String, Object> value = new LinkedHashMap<>();
            if (entry.getValue() instanceof Map<?, ?> rawMap) {
                rawMap.forEach((key, value1) -> value.put(key.toString(), value1));
            }
            for (Number key : keys) {
                map.put(key, value);
            }
        }
        return map;
    }

    @Override
    protected Map<String, Object> serialize(Map<Number, Map<String, Object>> value) {
        Map<String, Object> map = new LinkedHashMap<>();
        for (Map.Entry<Number, Map<String, Object>> entry : value.entrySet()) {
            map.put(Objects.toString(entry.getKey()), entry.getValue());
        }
        return map;
    }

    private Stream<Number> generateNumbers(String input) {
        if (Validate.isValidInteger(input)) {
            return Stream.of(Double.parseDouble(input));
        } else {
            String[] split = input.split("-", 2);
            Optional<BigDecimal> optional1 = Validate.getNumber(split[0].trim());
            Optional<BigDecimal> optional2 = Validate.getNumber(split[1].trim());
            if (optional1.isPresent() && optional2.isPresent()) {
                int s1 = optional1.get().intValue();
                int s2 = optional2.get().intValue();
                if (s1 <= s2) {
                    return IntStream.rangeClosed(s1, s2).mapToObj(Number.class::cast);
                } else {
                    return IntStream.rangeClosed(s2, s1).mapToObj(Number.class::cast).sorted(Collections.reverseOrder());
                }
            }
        }
        return Stream.empty();
    }

    private List<Number> getNumbers(String input) {
        return Stream.of(input.split(","))
                .flatMap(this::generateNumbers)
                .toList();
    }
}
