fn add(numbers: &str) -> Option<usize> {
    return numbers
        .split(|c| [',', '\n'].contains(&c))
        .map(|part: &str| part.parse::<usize>())
        .collect::<Result<Vec<usize>, _>>()
        .map(|numbers: Vec<usize>| numbers.into_iter().sum())
        .ok();
}

#[cfg(test)]
mod tests {
    use crate::add;

    #[test]
    fn sums_empty_string() {
        assert_eq!(add(""), None)
    }

    #[test]
    fn sums_single_number() {
        assert_eq!(add("42"), Some(42));
        assert_eq!(add("2"), Some(2));
    }

    #[test]
    fn fails_on_invalid_input() {
        assert_eq!(add("foo"), None);
    }

    #[test]
    fn sums_multiple_values() {
        assert_eq!(add("1,2"), Some(3))
    }

    #[test]
    fn supports_newline_as_separator() {
        assert_eq!(add("1\n2,3"), Some(6))
    }
}
