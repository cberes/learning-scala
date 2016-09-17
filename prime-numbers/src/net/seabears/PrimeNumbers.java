package net.seabears;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.IntStream;

public class PrimeNumbers {
	private static boolean isFactor(int x, int y) {
		return x % y == 0;
	}

	private static List<Integer> findPrimes(IntStream s) {
		final int[] numbers = s.toArray();
		if (numbers.length == 0) {
			return null;
		}

		final int prime = numbers[0];
		final List<Integer> primes = findPrimes(IntStream.of(numbers).skip(1).filter(n -> !isFactor(n, prime)));
		final List<Integer> allPrimes = new LinkedList<Integer>();
		allPrimes.add(prime);
		if (primes != null) {
			allPrimes.addAll(primes);
		}
		return allPrimes;
	}

	public static void main(String[] args) {
		System.out.println(findPrimes(IntStream.range(2, 1000)));
	}

}
